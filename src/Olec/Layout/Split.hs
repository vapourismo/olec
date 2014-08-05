module Olec.Layout.Split (
	--SplitInfo (..),
	--split,
) where


---- | Split Information
--data SplitInfo
--	= AbsVSplit Int    -- ^ Absolute Vertical Split
--	| AbsHSplit Int    -- ^ Absolute Horizontal Split
--	| RelVSplit Float  -- ^ Relative Veritcal Split
--	| RelHSplit Float  -- ^ Relative Horizontal Split

---- | Normalize the seperator
--normSep sep com
--	| sep < 0 = max 0 (min com (com + sep))
--	| otherwise = max 0 (min com sep)

---- | Generate an absolute seperator value from a relative seperator
--relSep sep com = toInt (sep * toFloat com) where
--	toFloat = fromInteger . toInteger
--	toInt = floor

---- | Generate two sub windows based on the layout information.
--split :: SplitInfo -> Window -> (Window, Window)

---- Absolute Vertical Split
--split (AbsVSplit sep') (Window x y w h) = (left, right) where
--	sep = normSep sep' w
--	left = Window x y sep h
--	right = Window (x + sep) y (w - sep) h

---- Absolute Horizontal Split
--split (AbsHSplit sep') (Window x y w h) = (top, bottom) where
--	sep = normSep sep' h
--	top = Window x y w sep
--	bottom = Window x (y + sep) w (h - sep)

---- Relative Vertical Split
--split (RelVSplit sep') (Window x y w h) = (left, right) where
--	sep = normSep (relSep sep' w) w
--	left = Window x y sep h
--	right = Window (x + sep) y (w - sep) h

---- Relative Horizontal Split
--split (RelHSplit sep') (Window x y w h) = (top, bottom) where
--	sep = normSep (relSep sep' h) h
--	top = Window x y w sep
--	bottom = Window x (y + sep) w (h - sep)
