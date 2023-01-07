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
import Streamly.Prelude qualified as Stream



main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  let bebe = Handle.read @IO
  qq <- Stream.unfold bebe stdin
                & Stream.chunksOf (256 * 1024) Fold.toList
                & fmap (B.take 32 . B.pack)
                & Stream.toList

  print (length qq)

