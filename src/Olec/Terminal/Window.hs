module Olec.Terminal.Window (Window, wMoveCursor, wDimension,
                             defaultWindow,
                             newWindow, subWindow,
                             fillWindow) where

import Olec.Terminal
import Control.Monad

type Window = (Int, Int, Int, Int)

-- | Position the cursor relative to the window's origin.
wMoveCursor :: Window -> Int -> Int -> IO ()
wMoveCursor (wx, wy, ww, wh) x y
	| x < ww && y < wh = moveCursor (wx + x) (wy + y)
wMoveCursor _ _ _ = return ()

-- | Retrieve the window's width and height.
wDimension :: Window -> (Int, Int)
wDimension (_, _, w, h) = (w, h)

-- | Fetch the default window (root window).
defaultWindow :: IO Window
defaultWindow = do
	(w, h) <- termDimension
	return (0, 0, w, h)

-- | Create a window within another window.
--   The given x- and y-coordinates are relative to the window's origin.
subWindow :: Window -> Int -> Int -> Int -> Int -> Window
subWindow (origX, origY, origW, origH) x' y' w' h' = (x, y, w, h) where
	x = min (origX + origW) (max origX x')
	y = min (origY + origH) (max origY y')
	w = min (origW - (x - origX)) w'
	h = min (origH - (y - origY)) h'

-- | Create an entirely new window.
newWindow :: Int -> Int -> Int -> Int -> IO Window
newWindow x y w h = do
	scr <- defaultWindow
	return (subWindow scr x y w h)

-- | Fill a window with a given character.
fillWindow :: Window -> Char -> IO ()
fillWindow win c = do
	let (w, h) = wDimension win
	forM_ [0 .. (h - 1)] (renderLine $ replicate w c) where
		renderLine line y = wMoveCursor win 0 y >> drawString line
