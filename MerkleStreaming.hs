{-# LANGUAGE MagicHash,UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class
import System.IO

import Data.Function
import Data.ByteString qualified as B
import Data.ByteString (ByteString)

import Streaming.Prelude qualified as S

readChunked :: MonadIO m => Handle -> Int -> S.Stream (S.Of ByteString) m ()
readChunked handle size = fix \next -> do
  chunk <- liftIO do
    B.hGet handle size
  unless (B.null chunk) do
    S.yield chunk
    next

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering

  qq <- S.toList_ do
      readChunked stdin (256*1024)
        & S.map do -- B.copy required otherwise we retain ref to the full BS -}
            B.copy . B.take 32

  print (length qq)

