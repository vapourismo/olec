module Olec.Terminal.Layout where

import Olec.Terminal.Window

-- | Split Information
data SplitInfo = AbsVSplit Int    -- ^ Absolute Vertical Split
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
split :: SplitInfo -> Window -> (Window, Window)

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

-- | Split Layout
data SplitLayout a = SplitLayout SplitInfo (SplitLayout a) (SplitLayout a)  -- ^ Split
                   | Renderer Window (a -> Window -> IO ())                 -- ^ Endpoint

-- | Fit a SplitLayout into a Window.
updateLayout :: SplitLayout a -> Window -> SplitLayout a
updateLayout (SplitLayout info a b) win =
	SplitLayout info (updateLayout a winA) (updateLayout b winB)
	where (winA, winB) = split info win
updateLayout (Renderer _ f) win = Renderer win f

-- | Render the layout.
renderLayout :: SplitLayout a -> a -> IO ()
renderLayout (SplitLayout _ a b) x = do
	renderLayout a x
	renderLayout b x
renderLayout (Renderer w f) x = f x w
