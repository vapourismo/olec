{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Olec.Terminal.Bindings
import Olec.Terminal.Input

main :: IO ()
main = withTerm $ do
	input <- processInput

	root <- defaultWindow
	addString root "Hello World"
	refreshWindow root

	void (readInputEvent input)
