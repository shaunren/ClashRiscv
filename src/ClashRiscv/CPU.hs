module ClashRiscv.CPU where

import Data.Maybe (fromMaybe, isJust)
import Control.Monad (guard)
import Clash.Prelude
import Clash.Intel.ClockGen

import ClashRiscv.Types
import ClashRiscv.Instructions
import ClashRiscv.ALU ( alu, aluCompare )
import ClashRiscv.ROM
import ClashRiscv.DataRAM
import ClashRiscv.MMIO

--createDomain vSystem{vName="DomInput", vPeriod=20000}
--createDomain vSystem{vName="Dom50", vPeriod=50000}

pipeline
  :: HiddenClockResetEnable dom
  => Signal dom (BitVector 8) -- LED signals
pipeline =
  let
    ---- Fetch
    if_pc = liftA2 fromMaybe if_pcReg ex_maybeJAddr
    if_pcReg = register 0 (liftA2 nextPC if_pc mem_loadStall)
      where
        nextPC pc stall
          | stall = pc
          | otherwise = pc + 4

    instrWord = instrRom "rom/rom.bin" $ fmap (truncateB . (`shiftR` 2)) if_pc

    ---- Decode
    id_pc' = register 0 if_pc
    id_pc = mux mem_loadStall ex_pc id_pc'

    -- TODO handle illegal ops
    maybe_instr = decode <$> instrWord
    instr = mux mem_loadStall id_instrOut (handleNop <$> maybe_instr)
    handleNop Nothing = Nop
    handleNop (Just inst)
      | isNop inst = Nop
      | otherwise  = inst
    instr_readRegs = extractReadRegs <$> instr

    id_regsOut = regFile instr_readRegs wb_writeReg
    id_instrOut = register Nop instr

    ---- Execute
    ex_pc = register 0 id_pc
    ex_sourceRegs = register (0, 0) instr_readRegs

    ex_instr = mux (mem_jumpStall .||. mem_loadStall) (pure Nop) id_instrOut
    
    ex_valsIn = fromMaybe <$> id_regsOut <*> (liftA2 (<|>) mem_maybeFwd wb_maybeFwd)
    
    (ex_out', ex_maybeJAddr) = unbundle $ liftA3 runExOp ex_instr ex_valsIn ex_pc
      where
        runExOp :: Instruction -> (Value, Value) -> Value -> (Value, Maybe Value)
        runExOp Nop _ _ = (0, Nothing)
        runExOp (Branch op _ _ imm) (x,y) pc
          | aluCompare op x y = (pc + 4, Just $ pc + offset)
          | otherwise         = (0, Nothing)
          where offset = unpack $ signExtend $ pack imm ++# (0 :: BitVector 1)
        runExOp (Store _ _ _ imm) (x,_) _ = (x + bitCoerce (signExtend imm), Nothing)
        runExOp (WithDstReg _ ins) (x,y) pc = case ins of
          (Lui imm)         -> (unpack $ pack imm ++# 0, Nothing)
          (Auipc imm)       -> (pc + unpack (pack imm ++# 0), Nothing)
          (Jal imm)         -> (pc + 4, Just $ pc + (unpack $ signExtend $ pack imm ++# (0 :: BitVector 1)))
          (Jalr _ imm)      -> (pc + 4, Just $ x + bitCoerce (signExtend imm))
          (Load _ _ imm)    -> (x + bitCoerce (signExtend imm), Nothing)
          (AluImm op _ imm) -> (alu op x (bitCoerce $ signExtend imm), Nothing)
          (AluReg op _ _)   -> (alu op x y, Nothing)
        runExOp _ _ _ = (0, Nothing) -- TODO: Fence ECall Ebreak

    ex_out = register 0 ex_out'

    ---- Memory
    -- mem_pc = register 0 ex_pc
    mem_instr = register Nop ex_instr
    mem_valsIn = register (0, 0) ex_valsIn

    mem_jumpStall = register False $ isJust <$> ex_maybeJAddr
    mem_loadStall = liftA2 shouldLoadStall mem_instr ex_sourceRegs
      where
        shouldLoadStall (WithDstReg rd' (Load {})) (rs1, rs2) =
          let rd = fromIntegral rd'
          in  rd /= 0 && (rd == rs1 || rd == rs2)
        shouldLoadStall _ _ = False

    mem_maybeFwd = mux (mem_jumpStall .||. mem_loadStall)
                       (pure Nothing)
                       (forwardReg <$> (fromIntegral . getDstReg <$> mem_instr) <*> ex_out <*> ex_sourceRegs <*> id_regsOut)

    mem_dataRamIn = runMemOp <$> mem_instr <*> ex_out <*> mem_valsIn
      where
        -- TODO check for misaligned access
        runMemOp (Store op _ _ _) addr (_, y) = DataRAMIn {
            addr = addr
          , wordType = case op of
              S_B -> DR_B
              S_H -> DR_H
              S_W -> DR_W
          , writeVal = Just y
          }
        runMemOp (WithDstReg _ (Load op _ _)) addr _ = DataRAMIn {
            addr = addr
          , wordType = case op of
              L_B  -> DR_B
              L_H  -> DR_H
              L_W  -> DR_W
              LU_B -> DR_BU
              LU_H -> DR_HU
          , writeVal = Nothing
          }
        runMemOp _ _ _ = def

    mem_out = register 0 ex_out

    ---- Writeback

    (wb_mmioVals, wb_maybeMmioOut) = handleMMIO mem_dataRamIn
    -- Only access data RAM if the RAM access is not MMIO.
    dataRamOut = dataRAM $ mux (isJust <$> wb_maybeMmioOut) (pure def) mem_dataRamIn

    wb_instr = register Nop mem_instr
    wb_writeReg = runWbOp <$> wb_instr <*> mem_out <*> dataRamOut
      where
        runWbOp :: Instruction -> Value -> DataRAMOut -> Maybe (RegAddr, Value)
        runWbOp (WithDstReg rd (Load {})) _ val = Just (bitCoerce rd, val)
        runWbOp (WithDstReg rd _)         val _ = Just (bitCoerce rd, val)
        runWbOp _ _ _ = Nothing

    wb_maybeFwd = wbForwardReg <$> wb_writeReg <*> ex_sourceRegs <*> id_regsOut
      where
        wbForwardReg wr rs ido = do
          (rd, x) <- wr
          forwardReg rd x rs ido

  in
    mmioLEDs <$> wb_mmioVals

-- |Extract register indicies to be read from an instruction.
extractReadRegs :: Instruction -> (RegAddr, RegAddr)
extractReadRegs (Branch _ rs1 rs2 _) = (fromIntegral rs1, fromIntegral rs2)
extractReadRegs (Store _ rs1 rs2 _)  = (fromIntegral rs1, fromIntegral rs2)
extractReadRegs (WithDstReg _ instr) = case instr of
  (Jalr rs _)        -> (fromIntegral rs, 0)
  (Load _ rs _)      -> (fromIntegral rs, 0)
  (AluImm _ rs _)    -> (fromIntegral rs, 0)
  (AluReg _ rs1 rs2) -> (fromIntegral rs1, fromIntegral rs2)
  _                  -> (0, 0)
extractReadRegs _ = (0, 0)

forwardReg :: RegAddr -> Value -> (RegAddr, RegAddr) -> (Value, Value) -> Maybe (Value, Value)
forwardReg rd x (rs1, rs2) (y, z)
  | rd == 0                = Nothing
  | rd == rs1 && rd /= rs2 = Just (x, z)
  | rd /= rs1 && rd == rs2 = Just (y, x)
  | rd == rs1 && rd == rs2 = Just (x, x)
  | otherwise = Nothing

--type RegFile = Vec 31 Value

type RegReadAddr = RegAddr
type RegWriteAddr = RegAddr

regFile :: HiddenClockResetEnable dom
  => Signal dom (RegReadAddr, RegReadAddr)
  -> Signal dom (Maybe (RegWriteAddr, Value))
  -> Signal dom (Value, Value)
regFile rd wr =
  let
    (rs1, rs2) = unbundle rd
    sinkR0 w = do
      (ix, v) <- w
      guard (ix /= 0)
      return (ix, v)
    wr' = sinkR0 <$> wr
    out1 = readNew (blockRamPow2 (replicate d32 0)) rs1 wr'
    out2 = readNew (blockRamPow2 (replicate d32 0)) rs2 wr'
  in
    bundle (out1, out2)
