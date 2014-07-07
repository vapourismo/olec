{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal (
	-- * General Types
	Position,
	Size,

	-- * Terminal Interaction
	withTerm,
	beginTerm,
	endTerm,

	-- * Properties
	termSize,
	wDimension,
	wOrigin,
	wEncloses,

	-- * Cursor
	moveCursor,
	cursor,
	wMoveCursor,

	-- * Drawing
	drawString,
	drawChar,
	drawByteString,
	wDrawChar,
	wDrawString,
	wDrawByteString,
 	wFill,
	clearTerm,
	clearToEOL,
	render,

	-- * Window Type
	Window,

	-- * Special Windows
	defaultWindow,
	nullWindow,

	-- * Window Creation
	newWindow,
	subWindow
) where

import Foreign.C
import Foreign.Marshal.Alloc

import Control.Applicative
import Control.Exception
import Control.Monad

import qualified Data.ByteString as B


-- Bindings for internal use.
foreign import ccall unsafe "terminal_width"
	_termWidth :: IO CInt

foreign import ccall unsafe "terminal_height"
	_termHeight :: IO CInt

foreign import ccall unsafe "terminal_cursor_x"
	_termCursorX :: IO CInt

foreign import ccall unsafe "terminal_cursor_y"
	_termCursorY :: IO CInt

foreign import ccall unsafe "terminal_move_cursor"
	_termMoveCursor :: CInt -> CInt -> IO ()

foreign import ccall unsafe "terminal_draw_char"
	_termDrawChar :: CChar -> IO ()

foreign import ccall unsafe "terminal_draw_string"
	_termDrawCString :: CString -> IO ()


-- | Initialize the terminal.
foreign import ccall unsafe "terminal_begin"
	beginTerm :: IO ()

-- | Finalize the terminal.
foreign import ccall unsafe "terminal_end"
	endTerm :: IO ()

-- | Clear the terminal.
foreign import ccall unsafe "terminal_clear"
	clearTerm :: IO ()

-- | Clear the rest of the line
foreign import ccall unsafe "terminal_clear_eol"
	clearToEOL :: IO ()

-- | Commit changes to the terminal.
foreign import ccall unsafe "terminal_render"
	render :: IO ()


-- | A cow.
type Size = (Int, Int)

-- | A sheep.
type Position = (Int, Int)


-- | Get the terminal width and height.
termSize :: IO Size
termSize = (,) <$> fmap cint2int _termWidth
               <*> fmap cint2int _termHeight where
	cint2int = fromInteger . toInteger

-- | Perform actions within an initialized terminal.
--   The terminal will be destroyed afterwards.
withTerm :: IO a -> IO a
withTerm = bracket_ beginTerm endTerm

-- | Draw a ByteString.
drawByteString :: B.ByteString -> IO ()
drawByteString bstr = B.useAsCString bstr _termDrawCString

-- | Draw a String.
drawString :: String -> IO ()
drawString s = do
	cstr <- newCString s
	_termDrawCString cstr
	free cstr

-- | Draw a Char.
drawChar :: Char -> IO ()
drawChar = _termDrawChar . castCharToCChar

-- | Get the cursor's position.
cursor :: IO Position
cursor = (,) <$> fmap cint2int _termCursorX
             <*> fmap cint2int _termCursorY where
	cint2int = fromInteger . toInteger

-- | Move the cursor to the given position.
moveCursor :: Int -> Int -> IO ()
moveCursor x y = _termMoveCursor (int2cint x) (int2cint y) where
	int2cint = fromInteger . toInteger


-- Data types
type Window = (Int, Int, Int, Int)


-- | An empty window
nullWindow :: Window
nullWindow = (0, 0, 0, 0)

-- | Position the cursor relative to the Window's origin.
wMoveCursor :: Window -> Position -> IO ()
wMoveCursor (wx, wy, _, _) (x, y) =
	moveCursor (wx + x) (wy + y)

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

-- | Fill a window with a given character.
wFill :: Window -> Char -> IO ()
wFill win c = forM_ [0 .. (h - 1)] (renderLine $ replicate w c) where
	(w, h) = wDimension win
	renderLine line y = wMoveCursor win (0, y) >> drawString line

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
