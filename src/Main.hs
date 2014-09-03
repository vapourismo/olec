{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Monad
import Olec.Terminal.Bindings
import Olec.Terminal.Input
import Olec.Visual
import Olec.BoxManager


fill :: Canvas a => a -> Char -> IO ()
fill c chr = do
	(_, _, w, h) <- getBounds c
	forM_ [0 .. h - 1] $ \y -> do
		setCursor c (0, y)
		drawString c (replicate (fromIntegral w) chr)

main :: IO ()
main = withTerm $ do
	input <- processInput
	(w, h) <- termSize

	win1 <- newWindow (0, 0) (5, 5)
	win2 <- newWindow (5, 5) (5, 5)
	win3 <- newWindow (10, 10) (5, 5)

	fill win1 '1'
	fill win2 '2'
	fill win3 '3'

	render win1
	render win2
	render win3

	void (readInputEvent input)

	clear win1
	clear win2
	clear win3

	render win1
	render win2
	render win3

	let wm =
		SplitVP 0.5
		        (Single win1)
		        (SplitHP 0.5
		                 (Single win2)
		                 (Single win3))

	setBounds wm (0, 0, w, h)

	fill win1 '1'
	fill win2 '2'
	fill win3 '3'

	render win1
	render win2
	render win3

	void (readInputEvent input)

	clear win1
	clear win2
	clear win3

	render win1
	render win2
	render win3

	setBounds wm (0, 0, div w 2, div h 2)

	fill win1 '1'
	fill win2 '2'
	fill win3 '3'

	render win1
	render win2
	render win3

	void (readInputEvent input)
