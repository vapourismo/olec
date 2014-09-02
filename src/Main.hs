{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Foreign.C.Types

import Olec.Terminal.Bindings
import Olec.Terminal.Input


main :: IO ()
main = withTerm $ do
	input <- processInput

	root <- defaultWindow
	win <- newWindow (5, 5) (20, 20)

	o <- origin win
	m <- maxXY win

	addString root (show (o, m))
	refreshWindow root

	void (readInputEvent input)
