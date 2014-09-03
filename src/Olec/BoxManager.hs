{-# LANGUAGE GADTs #-}

module Olec.BoxManager (
	BoxManager (..),

) where

import Control.Applicative
import Foreign.C.Types
import Olec.Visual


data BoxManager where
	SplitH :: (Box a, Box b) => CInt -> a -> b -> BoxManager
	SplitHP :: (Box a, Box b) => Float -> a -> b -> BoxManager
	SplitV :: (Box a, Box b) => CInt -> a -> b -> BoxManager
	SplitVP :: (Box a, Box b) => Float -> a -> b -> BoxManager
	Single :: Box a => a -> BoxManager


outerBounds :: Bounds -> Bounds -> Bounds
outerBounds (ax, ay, aw, ah) (bx, by, bw, bh) =
	(minx, miny, maxx - minx, maxy - miny) where
		minx = min ax bx
		miny = min ay by
		maxx = max (ax + aw - 1) (bx + bw - 1)
		maxy = max (ay + ah - 1) (by + bh - 1)

splitH :: (Box a, Box b) => CInt -> a -> b -> Bounds -> IO ()
splitH sep' a b (x, y, w, h) = do
	let sep = min (max sep' (- (h - 1))) (h - 1)
	let ab@(ax, ay, aw, ah) =
		if sep >= 0
			then (x, y, w, sep)
			else (x, y, w, h + sep)

	setBounds a ab
	setBounds b (ax, ay + ah, aw, h - ah)

splitV :: (Box a, Box b) => CInt -> a -> b -> Bounds -> IO ()
splitV sep' a b (x, y, w, h) = do
	let sep = min (max sep' (- (w - 1))) (w - 1)
	let ab@(ax, ay, aw, ah) =
		if sep >= 0
			then (x, y, sep, h)
			else (x, y, w + sep, h)

	setBounds a ab
	setBounds b (ax + aw, ay, w - aw, ah)


instance Box BoxManager where
	getBounds (Single box)    = getBounds box
	getBounds (SplitH _ a b)  = outerBounds <$> getBounds a <*> getBounds b
	getBounds (SplitHP _ a b) = outerBounds <$> getBounds a <*> getBounds b
	getBounds (SplitV _ a b)  = outerBounds <$> getBounds a <*> getBounds b
	getBounds (SplitVP _ a b) = outerBounds <$> getBounds a <*> getBounds b

	setBounds (Single box) r = setBounds box r
	setBounds (SplitH sep a b) r = splitH sep a b r
	setBounds (SplitHP f a b) (x, y, w, h) = splitH (round $ fromIntegral h * f) a b (x, y, w, h)
	setBounds (SplitV sep a b) r = splitV sep a b r
	setBounds (SplitVP f a b) (x, y, w, h) = splitV (round $ fromIntegral w * f) a b (x, y, w, h)
