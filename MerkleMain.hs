{-# LANGUAGE MagicHash,UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.Reader
import System.IO

import Data.Function
import Data.ByteString qualified as B
import Data.ByteString (ByteString)
import Data.IORef
import Data.Word

import Streamly.Data.Unfold qualified as Unfold
import Streamly.Data.Fold qualified as Fold
import Streamly.FileSystem.Handle qualified as Handle
import Streamly.Internal.Data.Array.Foreign.Type qualified as Array
import Streamly.Prelude qualified as Stream



main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  let bebe = Handle.read @IO
  qq <- Stream.unfold Handle.readChunksWithBufferOf (256*1024, stdin)
                & fmap (B.pack . Array.toList . fst . Array.splitAt 32)
                & Stream.toList 

  print (length qq)

