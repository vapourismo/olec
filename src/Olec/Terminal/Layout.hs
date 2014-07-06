module Olec.Terminal.Layout (
	SplitInfo (..),
	split,
	SplitProviderS,
	splitProvider
) where

import Olec.Terminal.Window
import Olec.Provider

import Control.Monad

-- | Split Information
data SplitInfo
	= AbsVSplit Int    -- ^ Absolute Vertical Split
	| AbsHSplit Int    -- ^ Absolute Horizontal Split
	| RelVSplit Float  -- ^ Relative Veritcal Split
	| RelHSplit Float  -- ^ Relative Horizontal Split

-- | Normalize the seperator
normSep sep com
	| sep < 0 = max 0 (min com (com + sep))
	| otherwise = max 0 (min com sep)

-- | Generate an absolute seperator value from a relative seperator
relSep sep com = toInt (sep * toFloat com) where
	toFloat = fromInteger . toInteger
	toInt = floor

-- | Generate two sub windows based on the layout information.
split :: SplitInfo -> Window -> (Window, Window)

-- Absolute Vertical Split
split (AbsVSplit sep') (x, y, w, h) = (left, right) where
	sep = normSep sep' w
	left = (x, y, sep, h)
	right = (x + sep, y, w - sep, h)

-- Absolute Horizontal Split
split (AbsHSplit sep') (x, y, w, h) = (top, bottom) where
	sep = normSep sep' h
	top = (x, y, w, sep)
	bottom = (x, y + sep, w, h - sep)

-- Relative Vertical Split
split (RelVSplit sep') (x, y, w, h) = (left, right) where
	sep = normSep (relSep sep' w) w
	left = (x, y, sep, h)
	right = (x + sep, y, w - sep, h)

-- Relative Horizontal Split
split (RelHSplit sep') (x, y, w, h) = (top, bottom) where
	sep = normSep (relSep sep' h) h
	top = (x, y, w, sep)
	bottom = (x, y + sep, w, h - sep)

-- | Split Provider State
data SplitProviderS s t a b
	= SplitProviderS SplitInfo (Window, Window) (Provider s a, Provider t b)

-- Update hook for a split provider
updateSplitProvider :: Window -> SplitProviderS s t a b -> SplitProviderS s t a b
updateSplitProvider win (SplitProviderS info _ (a, b)) = SplitProviderS info (winA, winB) (a', b') where
	(winA, winB) = split info win
	a' = updateProvider a winA
	b' = updateProvider b winB

renderSplitProvider :: Window -> SplitProviderS s t a b -> IO (a, b)
renderSplitProvider _ (SplitProviderS _ (winA, winB) (Provider stateA _ renderA, Provider stateB _ renderB)) =
	liftM2 (,) (renderA winA stateA) (renderB winB stateB)

-- | Split Provider
splitProvider :: SplitInfo -> Provider s a -> Provider t b -> Provider (SplitProviderS s t a b) (a, b)
splitProvider sinfo a b =
	Provider (SplitProviderS sinfo (nullWindow, nullWindow) (a, b)) updateSplitProvider renderSplitProvider
