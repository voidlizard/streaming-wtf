{-# LANGUAGE MagicHash,UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Data.Function
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Data.Int

import System.IO

splits :: Int64 -> LBS.ByteString -> [LBS.ByteString]
splits _ lbs
  | LBS.null lbs = []
splits size lbs =
  let (pre, post) = LBS.splitAt size lbs
  in pre: splits size post

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  input_data <- LBS.getContents
  let qq = input_data 
            & splits (256*1024)
            & fmap LBS.toStrict
            & fmap (B.copy . B.take 32)
  print (length qq)

