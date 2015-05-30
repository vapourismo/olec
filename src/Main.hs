{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.List

import qualified Data.Text as T

import Graphics.Text.Width

import Olec.Interface

import System.IO

-- | Entry point
main :: IO ()
main = do
	(_, handle, sizeIO) <- makeInterface

	size <- sizeIO
	runRenderer render handle size
	threadDelay 2000000

	where
		render = do
			moveCursor 1 1
			size <- getSize
			drawString (show size)
