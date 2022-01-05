module ClashRiscv.ROM (RomAddr, instrRom) where

import           Clash.Prelude
import           ClashRiscv.Types               ( Value )


type RomAddr = Unsigned 10

instrRom
    :: (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom RomAddr
    -> Signal dom Value
instrRom filepath addr = bitCoerce <$> romFile d1024 filepath addr
