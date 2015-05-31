{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Olec.Interface

-- | Entry point
main :: IO ()
main = do
	(_, output, sizeIO) <- makeInterface
	sizeIO >>= runRenderer renderer output
	threadDelay 2000000

	where
		renderer = do
			moveCursor 10 10
			setForegroundColor (Color 255 0 0)
			setBackgroundColor (Color 255 255 255)
			drawString "Herro"

			moveCursor 10 11
			setForegroundColor (Color 255 255 255)
			setBackgroundColor (Color 255 0 0)
			drawString "Werld"
