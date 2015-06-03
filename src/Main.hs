{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Olec.Interface.GTK
import Olec.Interface.Types
import Olec.Interface.Image

main :: IO ()
main = do
	d <- newInterface
	size <- dimensions d
	display d (0, 0) (drawText (Style (Color 255 0 0) (Color 255 255 255)) "Hello World" size)
	threadDelay 5000000
