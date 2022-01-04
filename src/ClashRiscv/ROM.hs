module ClashRiscv.ROM (RomAddr, instrRom) where

import           Data.Maybe                     ( fromMaybe )

import           Prelude                        ((++), replicate, length)
import           Clash.Prelude                  hiding ((++), replicate, length)
import qualified Clash.Sized.Vector             as V
import           ClashRiscv.Types               ( Value )


type RomAddr = Unsigned 10

instrRom
    :: (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom RomAddr
    -> Signal dom Value
instrRom filepath addr = bitCoerce <$> romFile d1024 filepath addr
