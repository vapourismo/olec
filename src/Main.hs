{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Olec.Interface

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
			drawString "Herro Werld"
