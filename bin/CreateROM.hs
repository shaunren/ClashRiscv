{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString as BS
import Data.Word (Word32)
import Text.Printf
import Control.Monad (forM_, replicateM_, when)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Bits (unsafeShiftL, (.|.))
import System.Environment (getArgs)


 -- https://stackoverflow.com/questions/48160345/efficient-conversion-of-bytestring-to-word16
parseBinaryWords :: BS.ByteString -> [Word32]
parseBinaryWords bs = fromMaybe [] $ do
  (b1, bs1) <- BS.uncons bs
  (b2, bs2) <- BS.uncons bs1
  (b3, bs3) <- BS.uncons bs2
  (b4, bs4) <- BS.uncons bs3
  let !res = fromIntegral b1
          .|. (fromIntegral b2 `unsafeShiftL` 8)
          .|. (fromIntegral b3 `unsafeShiftL` 16)
          .|. (fromIntegral b4 `unsafeShiftL` 24)
  Just (res : parseBinaryWords bs4)

main :: IO ()
main = do
  args <- getArgs
  let maybeFileSize = if length args /= 1 then Nothing else Just (read (head args) :: Int)
  
  words <- parseBinaryWords <$> BS.getContents

  forM_ words $ printf "%032b\n"
  when (isJust maybeFileSize) $
    replicateM_ (fromJust maybeFileSize - length words) $ putStrLn $ replicate 32 '0'
