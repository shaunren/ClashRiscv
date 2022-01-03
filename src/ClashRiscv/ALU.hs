{-# LANGUAGE DeriveGeneric  #-}
module ClashRiscv.ALU where

import GHC.Generics
import Clash.Prelude
import ClashRiscv.Types ( SignedValue, Value )

data ALUOp
  = Add
  | Sub
  | Sll
  | Slt
  | Sltu
  | Xor
  | Srl
  | Sra
  | Or
  | And
  | Mul
  | Mulh
  | Mulhsu
  | Mulhu
  | Div
  | Divu
  | Rem
  | Remu
  deriving (Show, Eq, Generic, NFDataX)

data BranchOp = B_EQ | B_NE | B_LT | B_GE | B_LTU | B_GEU deriving (Show, Eq, Generic, NFDataX)

alu :: ALUOp -> Value -> Value -> Value
alu op x1 x2 = case op of
  Add    -> bitCoerce $ s_x1 + s_x2
  Sub    -> bitCoerce $ s_x1 - s_x2
  Sll    -> x1 `shiftL` shamt
  Slt    -> if s_x1 < s_x2 then 1 else 0
  Sltu   -> if x1 < x2     then 1 else 0
  Xor    -> x1 `xor` x2
  Srl    -> x1 `shiftR` shamt
  Sra    -> bitCoerce $ s_x1 `shiftR` shamt
  Or     -> x1 .|. x2
  And    -> x1 .&. x2
  Mul    -> x1 * x2
  Mulh   -> getHigherHalfS $ s_x1 `mul` s_x2
  Mulhsu -> getHigherHalfS (signExtend s_x1 * bitCoerce (zeroExtend x2) :: Signed 64)
  Mulhu  -> unpack $ slice d63 d32 $ x1 `mul` x2

  -- TODO handle divide by zero
  Div -> bitCoerce $ s_x1 `div` s_x2
  Divu -> x1 `div` x2
  Rem -> bitCoerce $ s_x1 `mod` s_x2
  Remu -> x1 `mod` x2

  where
    s_x1  = bitCoerce x1 :: SignedValue
    s_x2  = bitCoerce x2 :: SignedValue
    shamt = fromIntegral (truncateB x2 :: Unsigned 5)
    getHigherHalfS = unpack . slice d63 d32

aluCompare :: BranchOp -> Value -> Value -> Bool
aluCompare B_EQ x y = x == y
aluCompare B_NE x y = x /= y
aluCompare B_LT x y = (bitCoerce x :: SignedValue) < (bitCoerce y :: SignedValue)
aluCompare B_GE x y = (bitCoerce x :: SignedValue) >= (bitCoerce y :: SignedValue)
aluCompare B_LTU x y = x < y
aluCompare B_GEU x y = x >= y
