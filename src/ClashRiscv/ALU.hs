{-# LANGUAGE DeriveGeneric  #-}
module ClashRiscv.ALU where

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
  deriving (Show, Eq, Generic, NFDataX)

data ALUMulOp
  = Mul
  | Mulh
  | Mulhsu
  | Mulhu
  deriving (Show, Eq, Generic, NFDataX)

data BranchOp = B_EQ | B_NE | B_LT | B_GE | B_LTU | B_GEU deriving (Show, Eq, Generic, NFDataX)

alu :: ALUOp -> Value -> Value -> Value
{-# INLINE alu #-}
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

  -- TODO implement division ops
  -- TODO handle divide by zero
  --Div -> bitCoerce $ s_x1 `div` s_x2
  --Divu -> x1 `div` x2
  --Rem -> bitCoerce $ s_x1 `mod` s_x2
  --Remu -> x1 `mod` x2

  where
    s_x1  = bitCoerce x1 :: SignedValue
    s_x2  = bitCoerce x2 :: SignedValue
    shamt = fromIntegral $ slice d4 d0 x2


aluCompare :: BranchOp -> Value -> Value -> Bool
aluCompare B_EQ x y = x == y
aluCompare B_NE x y = x /= y
aluCompare B_LT x y = (bitCoerce x :: SignedValue) < (bitCoerce y :: SignedValue)
aluCompare B_GE x y = (bitCoerce x :: SignedValue) >= (bitCoerce y :: SignedValue)
aluCompare B_LTU x y = x < y
aluCompare B_GEU x y = x >= y


-- | 3 cycle 32-bit * 32-bit -> 64-bit multiplier.
multiplier
  :: HiddenClockResetEnable  dom
  => Signal dom ALUMulOp
  -> Signal dom (Value, Value)
  -> Signal dom Value
{-# INLINE multiplier #-}
multiplier mulOpIn xyIn = out
  where    
    stage1F mulOp (x,y) = (mulOp, ll, hh, mid, xhCorr, yhCorr)
      where
        xl, xh, yl, yh :: Unsigned 16
        xl = bitCoerce $ slice d15 d0 x
        xh = bitCoerce $ slice d31 d16 x
        yl = bitCoerce $ slice d15 d0 y
        yh = bitCoerce $ slice d31 d16 y

        ll, lh, hl, hh :: Unsigned 32
        ll = xl `mul` yl
        lh = xl `mul` yh
        hl = xh `mul` yl
        hh = xh `mul` yh

        mid :: Unsigned 33
        mid = lh `add` hl

        xNeg = (mulOp == Mulh || mulOp == Mulhsu) && msb x == 1
        yNeg = mulOp == Mulh && msb y == 1

        xhCorr = if xNeg then y else 0
        yhCorr = if yNeg then x else 0

    stage1Out = register (Mul, 0, 0, 0, 0, 0) $ stage1F <$> mulOpIn <*> xyIn

    stage2F :: (ALUMulOp, Unsigned 32, Unsigned 32, Unsigned 33, Unsigned 32, Unsigned 32) -> (ALUMulOp, Unsigned 32, Unsigned 32, Unsigned 32)
    stage2F (mulOp, ll, hh, mid, xhCorr, yhCorr) = (mulOp, outl, outhU, hCorr)
      where
        outl' = ll `add` ((truncateB mid :: Unsigned 32) `shiftL` 16)
        outl  = truncateB outl'
        -- Unsigned upper half result
        outhU = (zeroExtend $ bitCoerce $ slice d32 d16 mid) + hh + fromIntegral (msb outl')
        hCorr = xhCorr + yhCorr

    stage2Out = register (Mul, 0, 0, 0) $ stage2F <$> stage1Out

    stage3F (mulOp, outl, outhU, hCorr)
      | mulOp == Mul = outl
      | otherwise    = outhU + hCorr
    out = stage3F <$> stage2Out
