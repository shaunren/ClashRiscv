{-# LANGUAGE DeriveGeneric #-}
module ClashRiscv.Instructions where

import Clash.Prelude

import ClashRiscv.Types
import ClashRiscv.ALU


type Imm20 = Signed 20
type Imm12 = Signed 12

type DstReg = Register

data DataOp = D_B | D_H | D_W | DU_B | DU_H deriving (Show, Eq, Generic, NFDataX)

class HasDstReg a where
  -- rd == 0 implies no dest register
  getDstReg :: a -> DstReg

data Instruction
  = Nop
  | Branch BranchOp Register Register Imm12
  | Store DataOp Register Register Imm12
  | Fence  -- TODO not implemented
  | ECall  -- TODO
  | EBreak -- TODO
  | WithDstReg DstReg InstructionWithDstReg
  deriving (Show, Generic, NFDataX)

instance HasDstReg Instruction where
  getDstReg (WithDstReg rd _) = rd
  getDstReg _                 = 0

data InstructionWithDstReg
  = Lui Imm20
  | Auipc Imm20
  | Jal Imm20
  | Jalr Register Imm12
  | Load DataOp Register Imm12
  | AluImm ALUIMOp Register Imm12
  | AluReg ALUIMOp Register Register
  deriving (Show, Generic, NFDataX)

data ALUIMOp
  = ALUI ALUOp
  | ALUM ALUMulOp
  | ALUD ALUDivOp
  deriving (Show, Generic, NFDataX)

decode :: Unsigned 32 -> Maybe Instruction
{-# INLINE decode #-}
decode word = case opcode of
  0b0110111 -> withRd $ Lui simm_u
  0b0010111 -> withRd $ Auipc simm_u
  0b1101111 -> withRd $ Jal simm_uj
  0b1100111 | funct3 == 0 -> withRd $ Jalr rs1 simm_i
  0b1100011
    | funct3 == 0b000 -> Just $ Branch B_EQ rs1 rs2 simm_sb
    | funct3 == 0b001 -> Just $ Branch B_NE rs1 rs2 simm_sb
    | funct3 == 0b100 -> Just $ Branch B_LT rs1 rs2 simm_sb
    | funct3 == 0b101 -> Just $ Branch B_GE rs1 rs2 simm_sb
    | funct3 == 0b110 -> Just $ Branch B_LTU rs1 rs2 simm_sb
    | funct3 == 0b111 -> Just $ Branch B_GEU rs1 rs2 simm_sb
    | otherwise       -> Nothing
  0b0000011
    | funct3 == 0b000 -> withRd $ Load D_B  rs1 simm_i
    | funct3 == 0b001 -> withRd $ Load D_H  rs1 simm_i
    | funct3 == 0b010 -> withRd $ Load D_W  rs1 simm_i
    | funct3 == 0b100 -> withRd $ Load DU_B rs1 simm_i
    | funct3 == 0b101 -> withRd $ Load DU_H rs1 simm_i
    | otherwise       -> Nothing
  0b0100011
    | funct3 == 0b000 -> Just $ Store D_B rs1 rs2 simm_s
    | funct3 == 0b001 -> Just $ Store D_H rs1 rs2 simm_s
    | funct3 == 0b010 -> Just $ Store D_W rs1 rs2 simm_s
    | otherwise       -> Nothing
  0b0010011 -> do
    aluOp <- decodeAluOp True funct3 funct7
    withRd $ AluImm aluOp rs1 simm_i
  0b0110011 -> do
    aluOp <- decodeAluOp False funct3 funct7
    withRd $ AluReg aluOp rs1 rs2
  0b0001111 -> Just Fence -- TODO decode parameters
  0b1110011
    | b31to20 == 0 -> Just ECall
    | b31to20 == 1 -> Just EBreak
    | otherwise    -> Nothing
  _  -> Nothing


  where
    opcode  = truncateB word :: Unsigned 7

    b11to7  = slice d11 d7 word
    b31to25 = slice d31 d25 word
    b31to12 = slice d31 d12 word
    b31to20 = slice d31 d20 word
    b19to15 = slice d19 d15 word
    b24to20 = slice d24 d20 word

    simm_i  = unpack b31to20
    simm_s  = unpack $ b31to25 ++# b11to7
    simm_u  = unpack b31to12
    simm_uj = unpack $ slice d31 d31 word ++# slice d19 d12 word ++# slice d20 d20 word ++# slice d30 d21 word
    simm_sb = unpack $ slice d31 d31 word ++# slice d7 d7 word   ++# slice d30 d25 word ++# slice d11 d8 word

    funct3  = slice d14 d12 word
    funct7  = b31to25

    rd      = unpack b11to7
    rs1     = unpack b19to15
    rs2     = unpack b24to20

    withRd  = Just . WithDstReg rd


decodeAluOp :: Bool -> BitVector 3 -> BitVector 7 -> Maybe ALUIMOp
{-# INLINE decodeAluOp #-}
decodeAluOp False funct3 1
  | funct3 < 4 = Just . ALUM $ (Mul :> Mulh :> Mulhsu :> Mulhu :> Nil) !! funct3
  | otherwise  = Just . ALUD $ (Div :> Divu :> Rem :> Remu :> Nil) !! (funct3 - 4)
decodeAluOp isImm funct3 funct7 = case funct3 of
  0b000
    | isImm || funct7 == 0 -> Just (ALUI Add)
    | funct7 == 0b0100000  -> Just (ALUI Sub)
    | otherwise            -> Nothing
  0b010 -> guardInt Slt
  0b011 -> guardInt Sltu
  0b100 -> guardInt Xor
  0b110 -> guardInt Or
  0b111 -> guardInt And
  0b001
    | funct7 == 0 -> Just (ALUI Sll)
    | otherwise   -> Nothing
  0b101
    | funct7 == 0          -> Just (ALUI Srl)
    | funct7 == 0b0100000  -> Just (ALUI Sra)
    | otherwise            -> Nothing
  _ -> Nothing

  where
    guardInt x = if isImm || funct7 == 0 then Just (ALUI x) else Nothing


isNop :: Instruction -> Bool
isNop Nop              = True
isNop (WithDstReg 0 (Jal _)) = False
isNop (WithDstReg 0 (Jalr _ _)) = False
isNop (WithDstReg 0 _) = True
isNop Fence            = True
isNop _                = False

isJump :: InstructionWithDstReg -> Bool
isJump (Jal {})  = True
isJump (Jalr {}) = True
isJump _         = False
