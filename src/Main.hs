{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Data.Metrics

import Olec.Interface.GTK
import Olec.Interface.Types
import Olec.Interface.Image

-- |
fillArea :: Style -> Char -> Visualiser
fillArea style c (w, h) =
	alignVertically hints (w, h)
	where
		line = replicate w c
		hints = replicate h (Absolute 1 (drawString style line))

main :: IO ()
main = do
	d <- newInterface

	clearOutput d
	outputImage d (0, 0) (fillArea (Style (Color 255 0 0) (Color 255 255 255)) 'x' (10, 10))
	threadDelay 5000000
