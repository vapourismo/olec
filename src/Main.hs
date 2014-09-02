{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Foreign.C.Types
import Olec.Terminal.Bindings
import Olec.Terminal.Input


main :: IO ()
main = withTerm $ do
	input <- processInput

	win <- newWindow (5, 5) (20, 20)

	o <- windowOrigin win
	m <- windowSize win

	addString win (show (o, m))
	refreshWindow win

	void (readInputEvent input)
