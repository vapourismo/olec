{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Monad
import Olec.Terminal.Bindings
import Olec.Terminal.Input
import Olec.Visual


fill :: Canvas a => a -> Char -> IO ()
fill c chr = do
	(_, _, w, h) <- getBounds c
	forM_ [0 .. h - 1] $ \y -> do
		setCursor c (0, y)
		drawString c (replicate (fromIntegral w) chr)

main :: IO ()
main = withTerm $ do
	input <- processInput

	void (readInputEvent input)
