module Olec.Terminal.Window (
	-- * Window Type
	Window,

	-- * Cursor
	wMoveCursor,

	-- * Properties
	wDimension,
	wOrigin,
	wEncloses,

	-- * Drawing
	wDrawChar,
	wDrawString,
	wDrawByteString,
	wDrawByteString8,

	-- * Special Windows
	defaultWindow,
	nullWindow,

	-- * Window Creation
	newWindow,
	subWindow,

	-- * Auxiliary
 	wFill
 ) where

import Olec.Terminal
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- Data types
type Window = (Int, Int, Int, Int)

-- | An empty window
nullWindow :: Window
nullWindow = (0, 0, 0, 0)

-- | Position the cursor relative to the Window's origin.
wMoveCursor :: Window -> Position -> IO ()
wMoveCursor (wx, wy, ww, wh) (x, y) =
	when (x < ww && y < wh) $ moveCursor (wx + x) (wy + y)

-- | Draw a String within a Window.
wDrawString :: Window -> String -> IO ()
wDrawString (winX, winY, winW, winH) str = do
	(x, y) <- cursor
	when (y >= winY && y < winY + winH && x < winX + winW && x + length str >= winX) $ do
		let (cutFront, cutBack, curX) = wFitEntity x winX winW (length str)
		let result = drop cutFront (take (length str - cutBack) str)
		when (length result > 0) $ do
			moveCursor curX y
			drawString result

-- | Draw a ByteString within a Window.
wDrawByteString :: Window -> B.ByteString -> IO ()
wDrawByteString (winX, winY, winW, winH) str = do
	(x, y) <- cursor
	when (y >= winY && y < winY + winH && x < winX + winW && x + B.length str >= winX) $ do
		let (cutFront, cutBack, curX) = wFitEntity x winX winW (B.length str)
		let result = B.drop cutFront (B.take (B.length str - cutBack) str)
		when (B.length result > 0) $ do
			moveCursor curX y
			drawByteString result

-- | Draw a ByteString.Char8 within a Window.
wDrawByteString8 :: Window -> C.ByteString -> IO ()
wDrawByteString8 (winX, winY, winW, winH) str = do
	(x, y) <- cursor
	when (y >= winY && y < winY + winH && x < winX + winW && x + C.length str >= winX) $ do
		let (cutFront, cutBack, curX) = wFitEntity x winX winW (C.length str)
		let result = C.drop cutFront (C.take (C.length str - cutBack) str)
		when (C.length result > 0) $ do
			moveCursor curX y
			drawByteString8 result

-- | Fit an entity inside a Window.
wFitEntity :: Int -> Int -> Int -> Int -> (Int, Int, Int)
wFitEntity x winX winW len =
	(cutFront, cutBack, max winX (min (winX + winW) x)) where
		cutFront = if x < winX
			then winX - x
			else 0
		cutBack = if x + len > winX + winW
			then (x + len) - (winX + winW)
			else 0

-- | Draw a character within the Window.
wDrawChar :: Window -> Char -> IO ()
wDrawChar win c =
	fmap (wEncloses win) cursor >>= flip when (drawChar c)

-- | Does the given Position lay within the given Window?
wEncloses :: Window -> Position -> Bool
wEncloses (x, y, w, h) (cx, cy) =
	x <= cx && cx < x + w
	&& y <= cy && cy < y + h

-- | Retrieve the Window's origin.
wOrigin :: Window -> Position
wOrigin (x, y, _, _) = (x, y)

-- | Retrieve the Window's width and height.
wDimension :: Window -> Size
wDimension (_, _, w, h) = (w, h)

-- | Fetch the default Window (root).
defaultWindow :: IO Window
defaultWindow = do
	(w, h) <- termSize
	return (0, 0, w, h)

-- | Create a Window within another Window.
--   The given x- and y-coordinates are relative to the Window's origin.
subWindow :: Window -> Position -> Size -> Window
subWindow (origX, origY, origW, origH) (x', y') (w', h') = (x, y, w, h) where
	x = max 0 $ min (origX + origW) (max origX x')
	y = max 0 $ min (origY + origH) (max origY y')
	w = max 0 $ min (origW - (x - origX)) w'
	h = max 0 $ min (origH - (y - origY)) h'

-- | Create an entirely new window.
newWindow :: Position -> Size -> IO Window
newWindow pos dim = do
	scr <- defaultWindow
	return (subWindow scr pos dim)

-- | Fill a window with a given character.
wFill :: Window -> Char -> IO ()
wFill win c = forM_ [0 .. (h - 1)] (renderLine $ replicate w c) where
	(w, h) = wDimension win
	renderLine line y = wMoveCursor win (0, y) >> drawString line
