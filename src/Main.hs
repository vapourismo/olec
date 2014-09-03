{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Control.Monad

import Olec.Terminal.Bindings
import Olec.Terminal.Input
import Olec.Visual


main :: IO ()
main = withTerm $ do
	input <- processInput

	win <- newWindow (5, 5) (20, 20)
	b <- getBounds win

	drawString win (show b)
	render win

	void (readInputEvent input)
