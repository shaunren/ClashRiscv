module ClashRiscv.UOps where

import           Clash.Prelude

import           ClashRiscv.ALU
import           ClashRiscv.Instructions        ( ALUIMOp(..)
                                                , Instruction(..)
                                                , InstructionWithDstReg(..)
                                                , LoadOp
                                                , StoreOp
                                                , isJump
                                                )
import           ClashRiscv.Types

data ExUOp
  = ExU_Nop
  | ExU_Branch BranchOp
  | ExU_Lui
  | ExU_Auipc
  | ExU_Jal
  | ExU_Jalr
  | ExU_AluImm ALUOp
  | ExU_AluReg ALUOp
  deriving (Show, Eq, Generic, NFDataX)

type ExMulUop = Maybe ALUMulOp

data MemUOp
  = MemU_Nop
  | MemU_Store StoreOp
  | MemU_Load LoadOp
  deriving (Show, Eq, Generic, NFDataX)

data WbUOp
  = WbU_Nop
  | WbU_WriteResult
  | WbU_WriteMulResult
  | WbU_WriteRAMOut
  deriving (Show, Eq, Generic, NFDataX)

data UOps = UOps
    { rdReg      :: Maybe RegAddr -- ^ isJust <=> rd is nonzero
    , immValue   :: Value
    , readyAtMem :: Bool  -- ^ Data will be ready at the beginning of MEM
    , exUOp      :: ExUOp
    , exMulUOp   :: ExMulUop
    , memUOp     :: MemUOp
    , wbUOp      :: WbUOp
    }
    deriving (Show, Eq, Generic, NFDataX)

instance Default UOps where
    def = UOps Nothing 0 True ExU_Nop Nothing MemU_Nop WbU_Nop


-- | Decodes an instruction into UOps.
decodeToUOps :: Instruction -> (UOps, (RegAddr, RegAddr))
decodeToUOps Nop = (def, (0, 0))
decodeToUOps (Branch op rs1 rs2 imm) =
    ( def { immValue = offset, exUOp = ExU_Branch op }
    , (bitCoerce rs1, bitCoerce rs2)
    )
    where offset = unpack $ signExtend $ pack imm ++# (0 :: BitVector 1)
decodeToUOps (Store op rs1 rs2 imm) =
    ( def { immValue = bitCoerce (signExtend imm), memUOp = MemU_Store op }
    , (bitCoerce rs1, bitCoerce rs2)
    )
decodeToUOps (WithDstReg 0 instr) | not (isJump instr) = (def, (0, 0))
decodeToUOps (WithDstReg rd instr) = (uops { rdReg = maybeRd }, rs)
  where
    maybeRd    = if rd == 0 then Nothing else Just (bitCoerce rd)

    (uops, rs) = case instr of
        (Lui imm) ->
            ( def { immValue = unpack (pack imm ++# 0)
                  , exUOp    = ExU_Lui
                  , wbUOp    = WbU_WriteResult
                  }
            , (0, 0)
            )
        (Auipc imm) ->
            ( def { immValue = unpack (pack imm ++# 0)
                  , exUOp    = ExU_Auipc
                  , wbUOp    = WbU_WriteResult
                  }
            , (0, 0)
            )
        (Jal imm) ->
            ( def { immValue = offset
                  , exUOp    = ExU_Jal
                  , wbUOp    = WbU_WriteResult
                  }
            , (0, 0)
            )
          where
            offset = unpack $ signExtend $ pack imm ++# (0 :: BitVector 1)
        (Jalr rs1 imm) ->
            ( def { immValue = bitCoerce (signExtend imm)
                  , exUOp    = ExU_Jalr
                  , wbUOp    = WbU_WriteResult
                  }
            , (bitCoerce rs1, 0)
            )
        (Load op rs1 imm) ->
            ( def { immValue   = bitCoerce (signExtend imm)
                  , readyAtMem = False
                  , memUOp     = MemU_Load op
                  , wbUOp      = WbU_WriteRAMOut
                  }
            , (bitCoerce rs1, 0)
            )
        (AluImm op rs1 imm) ->
            let (ALUI op') = op -- Only I instructions have imm values
            in  ( def { immValue = bitCoerce (signExtend imm)
                      , exUOp    = ExU_AluImm op'
                      , wbUOp    = WbU_WriteResult
                      }
                , (bitCoerce rs1, 0)
                )
        (AluReg op rs1 rs2) -> case op of
            (ALUI op') ->
                ( def { exUOp = ExU_AluReg op', wbUOp = WbU_WriteResult }
                , (bitCoerce rs1, bitCoerce rs2)
                )
            (ALUM op') ->
                ( def { readyAtMem = False           -- mul is complete at WB
                      , exMulUOp   = Just op'
                      , wbUOp      = WbU_WriteMulResult
                      }
                , (bitCoerce rs1, bitCoerce rs2)
                )
decodeToUOps _ = (def, (0, 0))
