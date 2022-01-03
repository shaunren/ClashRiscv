module ClashRiscv.DataRAM where

import Clash.Prelude
import Data.Maybe (isJust)
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
    ram = blockRamPow2 (replicate d1024 0)

    a = (complement 3 .&.) . truncateB . addr <$> dataRamIn
    wr' = calcWriteVal <$> dataRamIn
    wr = liftA2 (,) . Just <$> a <*> wr'

    ramOut = readNew ram a wr
    oldRamIn = register def dataRamIn

    

wordLength :: DataRAMIn -> Int
wordLength ramIn = case wordType ramIn of
      DR_B  -> 1
      DR_H  -> 2
      DR_W  -> 4
      DR_BU -> 1
      DR_HU -> 2

calcWriteVal :: DataRAMIn -> Maybe Value
calcWriteVal ramIn = do
  w <- writeVal ramIn
  let mask = (1 `shiftL` (8 * wordLength ramIn)) - 1
  return $ (w .&. mask) `shiftL` (8 * fromIntegral (3 .&. addr ramIn))

calcReadVal :: DataRAMIn -> DataRAMOut -> DataRAMOut
calcReadVal oldIn out
      | isJust (writeVal oldIn) = 0
      | otherwise = case wordType oldIn of
          DR_W -> out
          DR_B -> bitCoerce (signExtend (bitCoerce (truncateB rawVal :: Unsigned 8) :: Signed 8) :: Signed 32)
          DR_H -> bitCoerce (signExtend (bitCoerce (truncateB rawVal :: Unsigned 16) :: Signed 16) :: Signed 32)
          DR_BU -> zeroExtend (truncateB rawVal :: Unsigned 8)
          DR_HU -> zeroExtend (truncateB rawVal :: Unsigned 16)
      where
        -- Value before extension
        rawVal = out `shiftR` (8 * fromIntegral (3 .&. addr oldIn))
