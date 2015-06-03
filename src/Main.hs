{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import qualified Data.Text as T

import Olec.Interface.GTK
import Olec.Visual

-- |
fillArea :: Style -> Char -> Visualiser
fillArea style c (w, h) =
	VAlign (replicate h (Text style (T.pack (replicate w c))))

main :: IO ()
main = do
	d <- newInterface
	size <- dimensions d
	display d (0, 0) (fillArea (Style (Color 255 0 0) (Color 255 255 255)) 'x' (10, 10))
	threadDelay 5000000
