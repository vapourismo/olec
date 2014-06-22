module Olec.Terminal.Layout where

import Olec.Terminal.Window

data SplitLayout = AbsVSplit Int    -- ^ Absolute Vertical Split
                 | AbsHSplit Int    -- ^ Absolute Horizontal Split
                 | RelVSplit Float  -- ^ Relative Veritcal Split
                 | RelHSplit Float  -- ^ Relative Horizontal Split
	deriving Show

-- | Normalize the seperator
normSep sep com
	| sep < 0 = max 0 (min com (com + sep))
	| otherwise = max 0 (min com sep)

-- | Generate an absolute seperator value from a relative seperator
relSep sep com = toInt (sep * toFloat com) where
	toFloat = fromInteger . toInteger
	toInt = floor

-- | Generate two sub windows based on the layout information.
split :: SplitLayout -> Window -> (Window, Window)

-- Absolute Vertical Split
split (AbsVSplit sep') (x, y, w, h) = (left, right) where
	sep = normSep sep' w
	left = (x, y, sep, h)
	right = (x + sep, y, w - sep, h)

-- Absolute Horizontal Split
split (AbsHSplit sep') (x, y, w, h) = (left, right) where
	sep = normSep sep' h
	left = (x, y, w, sep)
	right = (x, y + sep, w, h - sep)

-- Relative Vertical Split
split (RelVSplit sep') (x, y, w, h) = (left, right) where
	sep = normSep (relSep sep' w) w
	left = (x, y, sep, h)
	right = (x + sep, y, w - sep, h)

-- Relative Horizontal Split
split (RelHSplit sep') (x, y, w, h) = (left, right) where
	sep = normSep (relSep sep' h) h
	left = (x, y, w, sep)
	right = (x, y + sep, w, h - sep)
