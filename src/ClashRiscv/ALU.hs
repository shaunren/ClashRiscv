{-# LANGUAGE DeriveGeneric  #-}
module ClashRiscv.ALU where

import Clash.Prelude
import ClashRiscv.Types ( SignedValue, Value, RegAddr )

import Data.Maybe (isNothing)

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

data ALUDivOp
  = Div
  | Divu
  | Rem
  | Remu
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
    stage1F mulOp (x,y) = (isMul, ll, hh, mid, xhCorr, yhCorr)
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

        isMul = mulOp == Mul

    stage1Out = register (True, 0, 0, 0, 0, 0) $ stage1F <$> mulOpIn <*> xyIn

    stage2F :: (Bool, Unsigned 32, Unsigned 32, Unsigned 33, Unsigned 32, Unsigned 32) -> (Bool, Unsigned 32, Unsigned 32, Unsigned 32)
    stage2F (isMul, ll, hh, mid, xhCorr, yhCorr) = (isMul, outl, outhU, hCorr)
      where
        outl' = ll `add` ((truncateB mid :: Unsigned 32) `shiftL` 16)
        outl  = truncateB outl'
        -- Unsigned upper half result
        outhU = (zeroExtend $ bitCoerce $ slice d32 d16 mid) + hh + fromIntegral (msb outl')
        hCorr = -(xhCorr + yhCorr)

    stage2Out = register (True, 0, 0, 0) $ stage2F <$> stage1Out

    stage3F (isMul, outl, outhU, hCorr)
      | isMul     = outl
      | otherwise = outhU + hCorr
    out = stage3F <$> stage2Out


data DividerState = DividerState
  { divRd                   :: Maybe RegAddr      -- Nothing implies the divider is idle.
  , divIter                 :: Unsigned 5         -- ^ Iteration counter
  , divSigned               :: Bool
  , divOutputRem            :: Bool
  , divPreMultipliedDivisors :: Vec 3 (Unsigned 34) -- ^ <3*divisor, 2*divisor, divisor>
  , divQuot                 :: Unsigned 32
  , divRem                  :: Unsigned 32
  } deriving (Generic, NFDataX)

instance Default DividerState where
  def = DividerState Nothing 0 False False (repeat 0) 0 0


-- | 16 cycle divider circuit that processes one input at a time.
divider
  :: HiddenClockResetEnable dom
  => Signal dom Bool                         -- ^ flush unit
  -> Signal dom (Maybe (RegAddr, ALUDivOp))
  -> Signal dom (Value, Value)
  -> Signal dom (Maybe RegAddr, Maybe Value) -- ^ if rd is Just, the divider is busy.
divider flush maybeRdAndOpIn xyIn =
  moore go output def $ bundle (flush, maybeRdAndOpIn, xyIn)

  where
    output :: DividerState -> (Maybe RegAddr, Maybe Value)
    output (DividerState maybeRd it _ outrem _ q r)
      | msb it == 1 = (maybeRd, Just $ if outrem then r else q)
      | otherwise   = (maybeRd, Nothing)

    go :: DividerState -> (Bool, Maybe (RegAddr, ALUDivOp), (Value, Value)) -> DividerState
    go state@(DividerState maybeRd it _ _ pmDivisors q r) (flush, maybeRdAndOp, (x,y))
      | flush || isNothing maybeRd || msb it == 1 = case maybeRdAndOp of
          Nothing       -> def
          Just (rd, op) -> newState
            where
              signed    = False -- TODO implement op == Div || op == Rem
              outputRem = op == Rem || op == Remu
              newState
                | y == 0 = DividerState (Just rd) 16 signed outputRem (repeat 0) (-1) x
                | signed && x == (1 `shiftL` 31) && y == -1 =
                    DividerState (Just rd) 16 signed outputRem (repeat $ -1) (1 `shiftL` 31) 0
                | otherwise =
                  let y2   = unpack $ pack y ++# (0 :: BitVector 1)
                      y3   = y2 `add` y
                      pmds = y3 :> zeroExtend y2 :> zeroExtend y :> Nil
                  in  DividerState (Just rd) 0 signed outputRem pmds x 0
      | otherwise =
        let pr        = unpack (slice d29 d0 r ++# slice d31 d30 q) :: Unsigned 32
            -- <pr - 3*d, pr - 2*d, pr - d>
            subs      = map (pr `sub`) pmDivisors
            -- cqb = max i in [0..3] with pr - i*d >= 0,
            -- r' = pr - cqb*d.
            (cqb,r') = case findIndex ((== 0) . msb) subs of
              Nothing   -> (0, pr)
              (Just ix) -> (3 - (pack ix :: BitVector 2), truncateB $ subs !! ix)
            q' = unpack $ slice d29 d0 q ++# cqb
        in  state { divIter = it + 1, divQuot = q', divRem = r' }

{-
dividerPipelined
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (RegAddr, ALUDivOp))
  -> Signal dom (Value, Value)
  -> Signal dom (Maybe (RegAddr, Value)) -- ^ fixed 16 cycle latency
dividerPipelined maybeRdAndOp opIn xyIn = getOutput <$> lastStage
  where
    pre = prepare <$> maybeRdAndOp <*> op <*> xyIn
    prepare Nothing _ _          = def
    prepare (Just (rd,op)) (x,y) = DividerState (Just rd) False signed outputRem y x 0
      where
        signed    = False -- TODO implement op == Div || op == Rem
        outputRem = op == Rem || op == Remu

    lastStage = foldl (\r i -> register def $ fmap (go (i == 16)) r) pre
              $ generate d16 (+1) (0 :: Int)

    getOutput state = do
      rd <- divRd state
      return $ (rd, if divOutputRem state then divRem state else divQuot state)

    go :: Bool -> DividerState -> DividerState
    go isLastStage state@(DividerState maybeRd _ _ _ divisor q r)
      | isNothing maybeRd = def
      | otherwise =
        let pr        = unpack (slice d29 d0 r ++# slice d31 d30 q) :: Unsigned 32
            subs      = map (\i -> (i, pr `sub` (divisor * zeroExtend i)))
                      $ generate d4 (subtract 1) (4 :: Unsigned 2)
            -- Find the leftmost (i, s) pair in subs with s >= 0.
            (cqb,r'') = fold f subs
              where f (i,s) (i',s')
                      | msb s  == 0 = (i,s)
                      | msb s' == 0 = (i',s')
                      | otherwise   = (i,s)
            q' = unpack $ slice d29 d0 q ++# pack cqb
            r' = truncateB r''
        in  state { divOutputReady = isLastStage, divQuot = q', divRem = r' }


-- | Quick-Div algorithm with variable latency
data DividerState = DividerState
  { divRunning        :: Bool
  , divOutputReady    :: Bool
  , divDivisor        :: Unsigned 32
  , divDivisorShifted :: Unsigned 32
  , divDivisorLz      :: Unsigned 5
  , divQuot    :: Unsigned 32
  , divRem     :: Unsgined 32
  } deriving (Generic, NFDataX)

instance Default DividerState where
  def = False False 0 0 0 0 0

-- | Calculates x / y given (x,y).
divider
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe (Value, Value))
  -> Signal dom (Vec 32 (Bool, Value, Value))
{-# INLINE divider #-}
divider maybeXyIn = _
  where
    pre = prepare <$> maybeXyIn
    prepare Nothing      = def
    prepare (Just (x,y)) = DividerState True False y yShifted (fromIntegral yLz) 0 x
      where
        yLz = countLeadingZeros y
        yShifted = y `shiftL` yLz

    stageStates = iterate d32 (register def . fmap go) pre
    stageOutputs = map (fmap (divOutputReady *** divQuot *** divRem)) stageStates

    go :: DividerState -> DividerState
    go state@(DividerState running ready divisor divisorShifted divisorLz q r)
      | ready || not running = def
      | r < divisor          = state { divRunning = False, divOutputReady = True }
      | otherwise =
        let rLz        = countLeadingZeros r
            estDivisor = divisorShifted `shiftR` rLz
            a          = r `sub` estDivisor
            aNeg       = msb a == 1
            b          = r - (estDivisor `shiftR` 1)
            clz_delta  = divisorLz - fromIntegral rLz
            q'         = setBit q (fromIntegral $ if aNeg then clz_delta - 1 else clz_delta)
            r'         = if aNeg then b else truncateB a
        in  state { divQuot = q', divRem = r' }
-}        
