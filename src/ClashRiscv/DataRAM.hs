{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ClashRiscv.DataRAM where

import Clash.Prelude
import ClashRiscv.Types ( Value, Addr )

data DataRAMWordType = DR_B | DR_H | DR_W | DR_BU | DR_HU deriving (Eq, Show, Generic, NFDataX)

data DataRAMIn = DataRAMIn { addr :: Addr, wordType :: DataRAMWordType, writeVal :: Maybe Value }
  deriving (Eq, Show, Generic, NFDataX)
instance Default DataRAMIn where
  def = DataRAMIn { addr = 0, wordType = DR_W, writeVal = Nothing }
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
calcWriteVal ramIn = do
  w <- writeVal ramIn
  return $ shiftWord (truncateWord w)

  where
    truncateWord w = case wordType ramIn of
      DR_W  -> w
      DR_B  -> zeroExtend (truncateB w :: Unsigned 8)
      DR_H  -> zeroExtend (truncateB w :: Unsigned 16)
      DR_BU -> zeroExtend (truncateB w :: Unsigned 8)
      DR_HU -> zeroExtend (truncateB w :: Unsigned 16)

    shiftWord w = case (truncateB (addr ramIn) :: Unsigned 2) of
      0 -> w
      1 -> w `shiftL` 8
      2 -> w `shiftL` 16
      3 -> w `shiftL` 24


calcReadVal :: DataRAMIn -> DataRAMOut -> DataRAMOut
{-# INLINE calcReadVal #-}
calcReadVal oldIn ramOut = case wordType oldIn of
  DR_W -> ramOut
  DR_B -> bitCoerce (signExtend (bitCoerce (truncateB rawVal :: Unsigned 8) :: Signed 8) :: Signed 32)
  DR_H -> bitCoerce (signExtend (bitCoerce (truncateB rawVal :: Unsigned 16) :: Signed 16) :: Signed 32)
  DR_BU -> zeroExtend (truncateB rawVal :: Unsigned 8)
  DR_HU -> zeroExtend (truncateB rawVal :: Unsigned 16)
  where
    -- Value before extension
    rawVal = case (truncateB (addr oldIn) :: Unsigned 2) of
      0 -> ramOut
      1 -> ramOut `shiftR` 8
      2 -> ramOut `shiftR` 16
      3 -> ramOut `shiftR` 24
