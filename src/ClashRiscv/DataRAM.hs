{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ClashRiscv.DataRAM where

import Clash.Prelude
import ClashRiscv.Types ( Value, Addr )
import ClashRiscv.Instructions ( DataOp(..) )

data DataRAMIn = DataRAMIn { addr :: Addr, dataOp :: DataOp, writeVal :: Maybe Value }
  deriving (Eq, Show, Generic, NFDataX)
instance Default DataRAMIn where
  def = DataRAMIn { addr = 0, dataOp = D_W, writeVal = Nothing }
type DataRAMOut  = Value -- Assumed available 1 cycle after holding DataRAMOut


dataRAM
  :: HiddenClockResetEnable dom
  => Signal dom DataRAMIn
  -> Signal dom DataRAMOut
dataRAM dataRamIn = calcReadVal <$> oldRamIn <*> ramOut
  where
    ram = blockRamPow2 $ replicate d1024 0

    wa = truncateB . (`shiftR` 2) . addr <$> dataRamIn
    wr' = calcWriteVal <$> dataRamIn
    wr = liftA2 (,) . Just <$> wa <*> wr'

    ramOut = readNew ram wa wr
    oldRamIn = register def dataRamIn


calcWriteVal :: DataRAMIn -> Maybe Value
{-# INLINE calcWriteVal #-}
calcWriteVal ramIn = writeVal ramIn -- FIXME implement byte enable
{-
do
  -- FIXME w needs to be combined with the preexisting ram value if not DR_W
  w <- writeVal ramIn
  return $ shiftWord (truncateWord w)

  where
    truncateWord w = case dataOp ramIn of
      D_W  -> w
      D_B  -> zeroExtend (truncateB w :: Unsigned 8)
      D_H  -> zeroExtend (truncateB w :: Unsigned 16)
      DU_B -> zeroExtend (truncateB w :: Unsigned 8)
      DU_H -> zeroExtend (truncateB w :: Unsigned 16)

    shiftWord w = case slice d1 d0 (addr ramIn) of
      0 -> w
      1 -> w `shiftL` 8
      2 -> w `shiftL` 16
      3 -> w `shiftL` 24
-}

calcReadVal :: DataRAMIn -> DataRAMOut -> DataRAMOut
{-# INLINE calcReadVal #-}
calcReadVal oldIn ramOut = case dataOp oldIn of
  D_W  -> ramOut
  D_B  -> bitCoerce $ signExtend $ slice d7 d0 rawVal
  D_H  -> bitCoerce $ signExtend $ slice d15 d0 rawVal
  DU_B -> bitCoerce $ zeroExtend $ slice d7 d0 rawVal
  DU_H -> bitCoerce $ zeroExtend $ slice d15 d0 rawVal
  where
    -- Value before extension
    rawVal = case slice d1 d0 (addr oldIn) of
      0 -> ramOut
      1 -> ramOut `shiftR` 8
      2 -> ramOut `shiftR` 16
      3 -> ramOut `shiftR` 24
