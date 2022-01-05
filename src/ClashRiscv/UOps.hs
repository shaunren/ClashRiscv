module ClashRiscv.UOps where

import           Clash.Prelude

import           ClashRiscv.ALU
import           ClashRiscv.Instructions        ( DstReg
                                                , Imm12
                                                , Imm20
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

data MemUOp
  = MemU_Nop
  | MemU_Store StoreOp
  | MemU_Load LoadOp
  deriving (Show, Eq, Generic, NFDataX)

data WbUOp
  = WbU_Nop
  | WbU_WriteResult
  | WbU_WriteRAMOut
  deriving (Show, Eq, Generic, NFDataX)

data UOps = UOps
    { immValue :: Value
    , exUOp    :: ExUOp
    , memUOp   :: MemUOp
    , wbUOp    :: WbUOp
    }
    deriving (Show, Eq, Generic, NFDataX)

instance Default UOps where
    def = UOps 0 ExU_Nop MemU_Nop WbU_Nop


-- | Decodes an instruction into UOps.
decodeToUOps :: Instruction -> (UOps, (Register, Register), DstReg)
decodeToUOps Nop = (def, (0, 0), 0)
decodeToUOps (Branch op rs1 rs2 imm) =
    ( def { immValue = unpack $ signExtend $ pack imm ++# (0 :: BitVector 1)
          , exUOp    = ExU_Branch op
          }
    , (rs1, rs2)
    , 0
    )
decodeToUOps (Store op rs1 rs2 imm) =
    ( def { immValue = bitCoerce (signExtend imm), memUOp = MemU_Store op }
    , (rs1, rs2)
    , 0
    )
decodeToUOps (WithDstReg 0 instr) | not (isJump instr) = (def, (0, 0), 0)
decodeToUOps (WithDstReg rd instr)                     = (uops, rs, rd)
  where
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
            ( def
                { immValue = unpack
                             $   signExtend
                             $   pack imm
                             ++# (0 :: BitVector 1)
                , exUOp    = ExU_Jal
                , wbUOp    = WbU_WriteResult
                }
            , (0, 0)
            )
        (Jalr rs1 imm) ->
            ( def { immValue = bitCoerce (signExtend imm)
                  , exUOp    = ExU_Jalr
                  , wbUOp    = WbU_WriteResult
                  }
            , (rs1, 0)
            )
        (Load op rs1 imm) ->
            ( def { immValue = bitCoerce (signExtend imm)
                  , memUOp   = MemU_Load op
                  , wbUOp    = WbU_WriteRAMOut
                  }
            , (rs1, 0)
            )
        (AluImm op rs1 imm) ->
            ( def { immValue = bitCoerce (signExtend imm)
                  , exUOp    = ExU_AluImm op
                  , wbUOp    = WbU_WriteResult
                  }
            , (rs1, 0)
            )
        (AluReg op rs1 rs2) ->
            (def { exUOp = ExU_AluReg op, wbUOp = WbU_WriteResult }, (rs1, rs2))
decodeToUOps _ = (def, (0, 0), 0)