module ClashRiscv.ROM (RomAddr, instrRom) where

import qualified Data.ByteString               as BS
import           Data.Maybe                     ( fromMaybe )

import           Prelude                        ((++), replicate, length)
import           Clash.Prelude                  hiding ((++), replicate, length)
import           System.IO.Unsafe               ( unsafePerformIO )
import qualified Clash.Sized.Vector            as V

import           ClashRiscv.Types               ( Value )


type RomAddr = Unsigned 10

readBinaryFile :: FilePath -> IO [Value]
readBinaryFile filepath = do
    contents <- BS.readFile filepath
    return $ parse contents
  where
    -- https://stackoverflow.com/questions/48160345/efficient-conversion-of-bytestring-to-word16
    parse bs = fromMaybe [] $ do
        (b1, bs1) <- BS.uncons bs
        (b2, bs2) <- BS.uncons bs1
        (b3, bs3) <- BS.uncons bs2
        (b4, bs4) <- BS.uncons bs3
        let !res = fromIntegral b1
                 + (fromIntegral b2 `unsafeShiftL` 8)
                 + (fromIntegral b3 `unsafeShiftL` 16)
                 + (fromIntegral b4 `unsafeShiftL` 24)
        Just (res : parse bs4)

instrRom
    :: (HiddenClockResetEnable dom)
    => FilePath
    -> Signal dom RomAddr
    -> Signal dom Value
instrRom filepath = rom romContents
  where
    binFile = unsafePerformIO $ readBinaryFile filepath
    romContents = V.unsafeFromList (binFile ++ replicate (max 0 (1024 - length binFile)) 0) :: Vec 1024 Value

