module ClashRiscv.CPU where

import Clash.Prelude
import Data.Maybe (fromMaybe, fromJust, isJust)
import Control.Monad (guard)

import Clash.Intel.ClockGen

import ClashRiscv.Types
import ClashRiscv.Instructions
import ClashRiscv.ALU ( alu, aluCompare, multiplier, divider, ALUMulOp(..), ALUDivOp(..) )
import ClashRiscv.ROM
import ClashRiscv.DataRAM
import ClashRiscv.UOps
import ClashRiscv.MMIO


createDomain vSystem{vName="DomInput", vPeriod=20000} -- 50 MHz
createDomain vSystem{vName="Dom150", vPeriod=6667}    -- 150 MHz


{-# ANN topEntity
  (Synthesize
    { t_name   = "cpu"
    , t_inputs = [PortName "CLOCK_50", PortName "RST"]
    , t_output = PortName "LED"
    }) #-}
topEntity
  :: Clock DomInput              -- ^ 50 MHz clock signal
  -> Signal DomInput Bool        -- ^ Reset signal
  -> Signal Dom150 (BitVector 8) -- ^ Output LED signals
topEntity clk rst = exposeClockResetEnable pipeline pllOut rstSync enableGen
  where
    -- FIXME The generated SystemVerilog has one less out clock port than expected.
    (pllOut, pllLocked) = alteraPll (SSymbol @"alteraPLL150") clk (unsafeFromLowPolarity rst)
    rstSync = resetSynchronizer pllOut (unsafeFromLowPolarity pllLocked) enableGen


pipeline
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 8)    -- ^ LED signals
pipeline =
  let
    ---- Fetch
    if_pc =  liftA2 fromMaybe (mux mem_bubbleEx if_prevPcReg if_pcReg) mem_maybeJAddr

    if_pcReg = register 0 $ fmap (+4) if_pc
    if_prevPcReg = register 0 if_pc -- Contains if_pc from the previous cycle.

    -- ROM: 1 cycle delay
    instrWord = instrRom "rom/rom.bin" $ fmap (truncateB . (`shiftR` 2)) if_pc

    ---- Decode
    -- In case EX needs to be bubbled, bring back the current instruction from EX.
    -- In case of jump stall, flush stage.
    id_pc' = register 0 if_pc
    id_pc = mux mem_bubbleEx id_prevPcReg id_pc'
    id_prevPcReg = register 0 id_pc

    -- TODO handle illegal ops
    id_uopsAndRs = handleStall <$> mem_jumpStall <*> mem_bubbleEx <*> id_prevUopsAndRsReg <*> instrWord
      where
        handleStall jumpStall bubbleEx prevUopsAndRs curInstrWord
          | jumpStall = def
          | bubbleEx  = prevUopsAndRs
          | otherwise = maybe def decodeToUOps $ decode curInstrWord
    id_prevUopsAndRsReg = register def id_uopsAndRs

    (id_uops, id_rs) = unbundle id_uopsAndRs
    id_bypassFrom = trackBypass <$> id_rs <*> ex_uops <*> ex_divEmitting <*> mem_prevUopsFromEx <*> mem_uops

    id_regsOut = regFile id_rs wb_writeReg

    ---- Execute
    ex_pc   = register 0 id_pc

    ex_uops' = register def id_uops
    ex_bypassFrom = register (BypassNone, BypassNone) id_bypassFrom

    -- Forward registers from WB and MEM if necessary.
    ex_valsIn = bypassReg <$> ex_bypassFrom <*> id_regsOut <*> mem_in <*> wb_in

    (ex_out', ex_maybeJAddr') = unbundle (runExOp <$> fmap exUOp ex_uops' <*> fmap immValue ex_uops' <*> ex_valsIn <*> ex_pc)
      where
        runExOp :: ExUOp -> Value -> (Value, Value) -> Value -> (Value, Maybe Value)
        runExOp ExU_Nop _ _ _               = (0, Nothing)
        runExOp (ExU_Branch op) imm (x,y) pc
          | aluCompare op x y               = (0, Just $ pc + imm)
          | otherwise                       = (0, Nothing)
        runExOp ExU_Lui imm _ _             = (imm, Nothing)
        runExOp ExU_Auipc imm _ pc          = (pc + imm, Nothing)
        runExOp ExU_Jal imm _ pc            = (pc + 4, Just $ pc + imm)
        runExOp ExU_Jalr imm (x,_) pc       = (pc + 4, Just $ x + imm)
        runExOp (ExU_AluImm op) imm (x,_) _ = (alu op x imm, Nothing)
        runExOp (ExU_AluReg op) _   (x,y) _ = (alu op x y, Nothing)

    ex_dataRamIn' = liftA3 runMemOp (memUOp <$> ex_uops') (immValue <$> ex_uops') ex_valsIn
      where
        -- TODO check for misaligned access
        runMemOp MemU_Nop _ _ = def
        runMemOp (MemU_Store op) imm (x,y) = DataRAMIn {
            addr = x + imm
          , wordType = case op of
              S_B -> DR_B
              S_H -> DR_H
              S_W -> DR_W
          , writeVal = Just y
          }
        runMemOp (MemU_Load op) imm (x,_)  = DataRAMIn {
            addr = x + imm
          , wordType = case op of
              L_B  -> DR_B
              L_H  -> DR_H
              L_W  -> DR_W
              LU_B -> DR_BU
              LU_H -> DR_HU
          , writeVal = Nothing
          }

    ex_mul' = multiplier (fromMaybe Mul . exMulUOp <$> ex_uops') ex_valsIn -- Ready in WB

    -- Calculcate stall signal in parallel with the ALU.
    stallEx = mem_jumpStall .||. mem_bubbleEx

    -- Flush the outputs if necessary.
    ex_uops       = mux stallEx (pure def) ex_uops'
    ex_maybeJAddr = mux stallEx (pure Nothing) ex_maybeJAddr'
    ex_dataRamIn  = mux stallEx (pure def) ex_dataRamIn'

    ex_prevDataRamIn = register def ex_dataRamIn

    -- divider: 16 cycles latency, 2 cycles latency for invalid inputs
    (ex_maybeDivRd, ex_maybeDiv) =
      unbundle $ divider flush (mux stallEx (pure Nothing) $ dividerIn <$> ex_uops') ex_valsIn
      where
        dividerIn u = do
          rd <- rdReg u
          op <- exDivUOp u
          return (rd, op)
        flush = (isJust . rdReg <$> ex_uops') .&&. (fmap rdReg ex_uops' .==. ex_maybeDivRd)
    ex_divBusy = isJust <$> ex_maybeDivRd
    ex_divEmitting = isJust <$> ex_maybeDiv

    -- Whether or not EX needs to be bubbled in the next cycle (excluding when div emits).
    ex_bubbleEx' = shouldBubble <$> ex_uops <*> ex_maybeDivRd <*> ex_divEmitting <*> id_rs
      where
        shouldBubble u maybeDivRd divEmitting (rs1, rs2) = needsWbOut || needsDivOut || needsDiv
          where
            needsWbOut = not (readyAtMem u) && maybe False (\rd -> rd == rs1 || rd == rs2) (rdReg u)
            needsDivOut = not divEmitting && maybe False (\rd -> rd == rs1 || rd == rs2) maybeDivRd
            needsDiv = isJust (exDivUOp u) && isJust (maybeDivRd)

    ex_out     = register 0 ex_out'
    ex_prevOut = register 0 ex_out

    ex_prevDivEmitting = register False ex_divEmitting

    ---- Memory
    mem_uopsFromEx = register def (mux stallEx def ex_uops)
    mem_prevUopsFromEx = register def mem_uopsFromEx

    mem_uops = selectUop <$> ex_prevDivEmitting <*> ex_divEmitting <*> ex_maybeDivRd <*> mem_uopsFromEx <*> mem_prevUopsFromEx
      where
        selectUop prevDivEmitting divEmitting maybeDivRd uopsFromEx prevUopsFromEx
          | divEmitting     =
            def { rdReg    = maybeDivRd
                , exDivUOp = Just Div   -- FIXME hack
                , wbUOp    = WbU_WriteResult
                }
          | prevDivEmitting = prevUopsFromEx
          | otherwise       = uopsFromEx

    mem_in = selectIn <$> ex_prevDivEmitting <*> ex_divEmitting <*> ex_maybeDiv <*> ex_out <*> ex_prevOut
      where
        selectIn prevDivEmitting divEmitting maybeDiv exOut exPrevOut
          | divEmitting     = fromJust maybeDiv
          | prevDivEmitting = exPrevOut
          | otherwise       = exOut

    mem_dataRamIn' = mux ex_prevDivEmitting ex_prevDataRamIn ex_dataRamIn -- NOTE: from EX
    -- Handle memory-mapped I/O first.
    (mem_mmioVals, mem_maybeMMIOOut) = mmio mem_dataRamIn'
    -- Only access data RAM if the RAM address is not memory mapped.
    mem_dataRamOut' = dataRAM $ mux (isJust <$> mem_maybeMMIOOut) (pure def) mem_dataRamIn'

    mem_maybeJAddr = register Nothing ex_maybeJAddr

    mem_jumpStall = isJust <$> mem_maybeJAddr
    mem_bubbleEx = (register False ex_bubbleEx') .||. ex_divEmitting

    mem_dataRamOut = register 0 $ liftA2 fromMaybe mem_dataRamOut' mem_maybeMMIOOut
    mem_out = register 0 mem_in

    ---- Writeback
    wb_uops = register def mem_uops

    wb_in = selectIn <$> fmap wbUOp wb_uops <*> mem_out <*> ex_mul' <*> mem_dataRamOut 
      where
        selectIn WbU_WriteMulResult _ mulOut _  = mulOut
        selectIn WbU_WriteRAMOut _ _ dataRamOut = dataRamOut
        selectIn _ out _ _                      = out

    
    wb_writeReg = liftA3 runWbOp (wbUOp <$> wb_uops) (rdReg <$> wb_uops) wb_in
      where
        runWbOp WbU_Nop _ _   = Nothing
        runWbOp _ maybeRd val = maybeRd >>= \rd -> Just (rd, val)
  in
    mmioLEDs <$> mem_mmioVals


data BypassFrom = BypassNone | BypassMem | BypassWb
  deriving (Eq, Show, Generic, NFDataX)

trackBypass
  :: (RegReadAddr, RegReadAddr)
  -> UOps                        -- ^ EX stage UOps
  -> Bool                        -- ^ MEM div emitting
  -> UOps                        -- ^ Saved prev EX UOps
  -> UOps                        -- ^ MEM stage UOps
  -> (BypassFrom, BypassFrom)
{-# INLINE trackBypass #-}
trackBypass (rs1, rs2) exUOps memDivEmitting prevExUOps memUOps = (b1, b2)
  where
    (b1', b2') = fromMaybe (BypassNone, BypassNone) $ do
      rd <- rdReg memUOps
      return ( if rd == rs1 then BypassWb else BypassNone
             , if rd == rs2 then BypassWb else BypassNone
             )
    (b1, b2) = fromMaybe (b1', b2') $ do
      let u = if memDivEmitting then prevExUOps else exUOps
      guard $ readyAtMem u
      rd <- rdReg u
      return ( if rd == rs1 then BypassMem else b1'
             , if rd == rs2 then BypassMem else b2'
             )

  
bypassReg
  :: (BypassFrom, BypassFrom)
  -> (Value, Value)
  -> Value                     -- ^ Data from MEM
  -> Value                     -- ^ Data from WB
  -> (Value, Value)
{-# INLINE bypassReg #-}
bypassReg (b1, b2) (x1, x2) memx wbx =
  (selectVal b1 x1 memx wbx, selectVal b2 x2 memx wbx)
  where
    selectVal b x mx wx = case b of
      BypassNone -> x
      BypassMem  -> mx
      BypassWb   -> wx


regFile :: HiddenClockResetEnable dom
  => Signal dom (RegReadAddr, RegReadAddr)
  -> Signal dom (Maybe (RegWriteAddr, Value))
  -> Signal dom (Value, Value)
regFile rs wr =
  let
    (rs1, rs2) = unbundle rs
    out1 = readNew (blockRamPow2 (replicate d32 0)) rs1 wr
    out2 = readNew (blockRamPow2 (replicate d32 0)) rs2 wr
  in
    bundle (out1, out2)
