module Olec.Layout.Pane (
	-- * Pane
	Pane (..),
	rootPane,

	-- * Bounds checking
	paneSize,
	withinPane,
	fitIntoPane,
) where

import Olec.Terminal
import Control.Applicative
--import qualified Data.ByteString as B


-- | A cat.
data Pane = Pane { paneOrigin  :: (Int, Int)
                 , paneStopper :: (Int, Int) }

-- | Root pane.
rootPane :: IO Pane
rootPane = Pane (0, 0) <$> termSize

-- | Check if the given absolute coordinates lie within the given pane.
withinPane :: (Int, Int) -> Pane -> Bool
withinPane (x, y) (Pane (ox, oy) (sx, sy)) =
	x >= ox && y >= oy && x < sx && y < sy

-- | Get the pane's width and height.
paneSize :: Pane -> (Int, Int)
paneSize (Pane (ox, oy) (sx, sy)) = (sx - ox, sy - oy)

-- | Fit an entity within a pane.
fitIntoPane :: Int -> Int -> Pane -> Maybe (Int, Int)
fitIntoPane x w (Pane (ox, _) (sx, _))
	| left < sx && len > 0 = Just (left, len)
	| otherwise = Nothing
	where
		left = max x ox
		right = min sx (x + w)
		len = right - left
