module ClashRiscv.MMIO where

import           Clash.Prelude
import           ClashRiscv.DataRAM             ( DataRAMIn(..)
                                                , calcReadVal
                                                , calcWriteVal
                                                )
import           ClashRiscv.Types               ( Value )
import           Control.Monad                  ( guard )
import           Data.Maybe                     ( fromMaybe )


data MMIOData = MMIOData { mmioLEDs :: BitVector 8 }
  deriving (Show, Default, Generic, NFDataX)


mmio
  :: HiddenClockResetEnable dom
  => Signal dom DataRAMIn
  -> (Signal dom MMIOData, Signal dom (Maybe Value)) -- ^ Delayed by 1 cycle.
mmio dataRamIn = (mmioVals, out)
  where
    isMMIO   = fmap addr dataRamIn .==. 0x40000000

    mmioVals = register def $ liftA3 updateVals dataRamIn isMMIO mmioVals

    updateVals ramIn is_mmio vals = fromMaybe vals $ do
        guard is_mmio
        wr <- calcWriteVal ramIn
        return $ vals { mmioLEDs = truncateB (bitCoerce wr) }

    out = register Nothing $ liftA2
        orNothing
        isMMIO
        (   calcReadVal
        <$> dataRamIn
        <*> fmap (bitCoerce . zeroExtend . mmioLEDs) mmioVals
        )

-- | Produce a 'Just' when predicate is True, else Nothing.
orNothing :: Bool -> a -> Maybe a
orNothing True  a = Just a
orNothing False _ = Nothing
