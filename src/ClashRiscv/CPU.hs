module ClashRiscv.CPU where

import Clash.Prelude
import Data.Maybe (fromMaybe, isJust)
import Control.Monad (guard)
import Control.Arrow ((***))


import Clash.Intel.ClockGen

import ClashRiscv.Types
import ClashRiscv.Instructions
import ClashRiscv.ALU ( alu, aluCompare )
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
    if_pc = mux mem_loadStall if_prevPcReg (liftA2 fromMaybe if_pcReg mem_maybeJAddr)
    if_pcReg = register 0 $ fmap (+4) if_pc
    if_prevPcReg = register 0 if_pcReg      -- if_pcReg delayed by 1 cycle

    -- ROM: 1 cycle delay
    instrWord = instrRom "rom/rom.bin" $ fmap (truncateB . (`shiftR` 2)) if_pc

    ---- Decode
    -- In case of load stall, bring back the instruction from EX and bubble EX.
    -- In case of jump stall, flush stage.
    id_pc' = register 0 if_pc
    id_pc = mux mem_loadStall ex_pc id_pc'

    -- TODO handle illegal ops
    id_uopsAndRs = handleStall <$> mem_jumpStall <*> mem_loadStall <*> id_prevUopsAndRsReg <*> instrWord
      where
        handleStall jumpStall loadStall prevUopsAndRs curInstrWord
          | jumpStall = def
          | loadStall = prevUopsAndRs
          | otherwise = maybe def decodeToUOps $ decode curInstrWord
    id_prevUopsAndRsReg = register def id_uopsAndRs

    (id_uops, id_rs', id_rd') = unbundle id_uopsAndRs
    id_rs = fmap (bitCoerce *** bitCoerce) id_rs'
    id_rd = fmap bitCoerce id_rd'

    id_regsOut = regFile id_rs wb_writeReg

    ---- Execute
    ex_pc   = register 0 id_pc
    ex_rs   = register (0, 0) id_rs

    stallEx = mem_jumpStall .||. mem_loadStall
    ex_rd   = mux stallEx (pure 0)   $ register 0 id_rd
    ex_uops = mux stallEx (pure def) $ register def id_uops

    -- Forward registers from WB and MEM if necessary.
    ex_valsIn = forwardReg <$> memFwd <*> wbFwd <*> ex_rs <*> id_regsOut
      where
        memFwd = bundle (mem_rd, ex_out)
        wbFwd  = bundle (wb_rd, wb_writeVal)

    (ex_out', ex_maybeJAddr') = unbundle (runExOp <$> fmap exUOp ex_uops <*> fmap immValue ex_uops <*> ex_valsIn <*> ex_pc)
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

    ex_dataRamIn = liftA3 runMemOp (memUOp <$> ex_uops) (immValue <$> ex_uops) ex_valsIn
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

    ex_out = register 0 ex_out'

    ---- Memory
    mem_uops = register def ex_uops
    mem_rd   = register 0 ex_rd

    -- Handle memory-mapped I/O first.
    (mem_mmioVals, mem_maybeMMIOOut) = mmio ex_dataRamIn
    -- Only access data RAM if the RAM address is not memory mapped.
    mem_dataRamOut = dataRAM $ mux (isJust <$> mem_maybeMMIOOut) (pure def) ex_dataRamIn

    mem_maybeJAddr = register Nothing ex_maybeJAddr'

    mem_jumpStall = isJust <$> mem_maybeJAddr
    mem_loadStall = liftA3 shouldLoadStall (memUOp <$> mem_uops) mem_rd ex_rs
      where
        shouldLoadStall (MemU_Load {}) rd (rs1, rs2) = rd == rs1 || rd == rs2
        shouldLoadStall _ _ _ = False

    mem_writeVal' = mux (useRAMOut <$> mem_uops)
                   (liftA2 fromMaybe mem_dataRamOut mem_maybeMMIOOut)
                   ex_out
      where useRAMOut = (WbU_WriteRAMOut ==) . wbUOp

    ---- Writeback
    wb_writeVal = register 0 mem_writeVal'

    wb_uops = register def mem_uops
    wb_rd   = register 0 mem_rd
    
    wb_writeReg = liftA3 runWbOp (wbUOp <$> wb_uops) wb_rd wb_writeVal
      where
        runWbOp WbU_Nop _ _   = Nothing
        runWbOp _ rd val      = Just (rd, val)

  in
    mmioLEDs <$> mem_mmioVals


type RegReadAddr  = RegAddr
type RegWriteAddr = RegAddr

forwardReg
  :: (RegWriteAddr, Value)
  -> (RegWriteAddr, Value)
  -> (RegReadAddr, RegReadAddr)
  -> (Value, Value)
  -> (Value, Value)
{-# INLINE forwardReg #-}
forwardReg (memrd, memx) (wbrd, wbx) (rs1, rs2) (x, y) = (x', y')
  where
    x' | rs1 == 0     = 0
       | memrd == rs1 = memx
       | wbrd == rs1  = wbx
       | otherwise    = x

    y' | rs2 == 0     = 0
       | memrd == rs2 = memx
       | wbrd == rs2  = wbx
       | otherwise    = y


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
