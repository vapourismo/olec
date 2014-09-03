{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Monad
import Control.Applicative

import Foreign.C.Types

import Olec.Terminal.Bindings
import Olec.Terminal.Input
import Olec.Visual


data WindowManager where
	SplitH :: (Box a, Box b) => CInt -> a -> b -> WindowManager
	SplitV :: (Box a, Box b) => CInt -> a -> b -> WindowManager
	Single :: Box a => a -> WindowManager

outerBounds :: Bounds -> Bounds -> Bounds
outerBounds (ax, ay, aw, ah) (bx, by, bw, bh) =
	(minx, miny, maxx - minx, maxy - miny) where
		minx = min ax bx
		miny = min ay by
		maxx = max (ax + aw - 1) (bx + bw - 1)
		maxy = max (ay + ah - 1) (by + bh - 1)


instance Box WindowManager where
	getBounds (Single box) = getBounds box
	getBounds (SplitH _ a b) = outerBounds <$> getBounds a <*> getBounds b
	getBounds (SplitV _ a b) = outerBounds <$> getBounds a <*> getBounds b

	setBounds (Single box) b = setBounds box b
	setBounds (SplitH sep' a b) (x, y, w, h) = do
		let sep = min (max sep' (- (h - 1))) (h - 1)
		let ab@(ax, ay, aw, ah) =
			if sep >= 0
				then (x, y, w, sep)
				else (x, y, w, h + sep)

		setBounds a ab
		setBounds b (ax, ay + ah, aw, h - ah)
	setBounds (SplitV sep' a b) (x, y, w, h) = do
		let sep = min (max sep' (- (w - 1))) (w - 1)
		let ab@(ax, ay, aw, ah) =
			if sep >= 0
				then (x, y, sep, h)
				else (x, y, w + sep, h)

		setBounds a ab
		setBounds b (ax + aw, ay, w - aw, ah)


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

	let wm =
		SplitV (div w 2)
		       (Single win1)
		       (SplitH (div h 2)
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
