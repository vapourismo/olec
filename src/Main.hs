{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Olec.Interface

import System.Posix.IO

import qualified Data.ByteString as B

-- | Entry point
main :: IO ()
main = do
	(_, outputFd, _) <- makeRawInterface
	handle <- fdToHandle outputFd

	B.hPut handle "Hello World"

	threadDelay 1000000
