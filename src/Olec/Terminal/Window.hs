module Olec.Terminal.Window (Window, wMoveCursor,
                             wDimension, wOrigin,
                             defaultWindow,
                             newWindow, subWindow,
                             fillWindow) where

import Olec.Terminal
import Control.Monad

-- Data types
type Window = (Int, Int, Int, Int)
type Dimension = (Int, Int)
type Position = (Int, Int)

-- | Position the cursor relative to the window's origin.
wMoveCursor :: Window -> Position -> IO ()
wMoveCursor (wx, wy, ww, wh) (x, y)
	| x < ww && y < wh = moveCursor (wx + x) (wy + y)
wMoveCursor _ _ = return ()

-- | Retrieve the window's origin.
wOrigin :: Window -> Position
wOrigin (x, y, _, _) = (x, y)

-- | Retrieve the window's width and height.
wDimension :: Window -> Dimension
wDimension (_, _, w, h) = (w, h)

-- | Fetch the default window (root window).
defaultWindow :: IO Window
defaultWindow = do
	(w, h) <- termDimension
	return (0, 0, w, h)

-- | Create a window within another window.
--   The given x- and y-coordinates are relative to the window's origin.
subWindow :: Window -> Position -> Dimension -> Window
subWindow (origX, origY, origW, origH) (x', y') (w', h') = (x, y, w, h) where
	x = max 0 $ min (origX + origW) (max origX x')
	y = max 0 $ min (origY + origH) (max origY y')
	w = max 0 $ min (origW - (x - origX)) w'
	h = max 0 $ min (origH - (y - origY)) h'

-- | Create an entirely new window.
newWindow :: Position -> Dimension -> IO Window
newWindow pos dim = do
	scr <- defaultWindow
	return (subWindow scr pos dim)

-- | Fill a window with a given character.
fillWindow :: Window -> Char -> IO ()
fillWindow win c = forM_ [0 .. (h - 1)] (renderLine $ replicate w c) where
	(w, h) = wDimension win
	renderLine line y = wMoveCursor win (0, y) >> drawString line

