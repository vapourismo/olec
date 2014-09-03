{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Olec.Terminal.Bindings (
	-- * Types
	Window,

	-- * Basics
	withTerm,
	termSize,

	-- * Windows
	newWindow,
) where

import Control.Exception

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr

import Olec.Visual


-- Initialization and finalization
foreign import ccall unsafe "olec_init"
	initTerm :: IO ()

foreign import ccall unsafe "endwin"
	disposeTerm :: IO ()

foreign import ccall unsafe "olec_width"
	termWidth :: IO CInt

foreign import ccall unsafe "olec_height"
	termHeight :: IO CInt

-- Window Interaction
foreign import ccall unsafe "newwin"
	newWindow_ :: CInt -> CInt -> CInt -> CInt -> IO (Ptr RawWindow)

foreign import ccall unsafe "&delwin"
	deleteWindow_ :: FunPtr (Ptr RawWindow -> IO ())

foreign import ccall unsafe "mvwin"
	moveWindow_ :: Ptr RawWindow -> CInt -> CInt -> IO ()

foreign import ccall unsafe "wresize"
	resizeWindow_ :: Ptr RawWindow -> CInt -> CInt -> IO ()

-- Window Properties
foreign import ccall unsafe "getbegy"
	windowY :: Ptr RawWindow -> IO CInt

foreign import ccall unsafe "getbegx"
	windowX :: Ptr RawWindow -> IO CInt

foreign import ccall unsafe "getmaxy"
	windowMaxY :: Ptr RawWindow -> IO CInt

foreign import ccall unsafe "getmaxx"
	windowMaxX :: Ptr RawWindow -> IO CInt

-- Cursor
foreign import ccall unsafe "getcury"
	windowCursorY :: Ptr RawWindow -> IO CInt

foreign import ccall unsafe "getcurx"
	windowCursorX :: Ptr RawWindow -> IO CInt

foreign import ccall unsafe "wmove"
	moveCursor_ :: Ptr RawWindow -> CInt -> CInt -> IO ()

-- Rendering
foreign import ccall unsafe "waddstr"
	addString_ :: Ptr RawWindow -> CString -> IO ()

foreign import ccall unsafe "wrefresh"
	refreshWindow_ :: Ptr RawWindow -> IO ()

foreign import ccall unsafe "wclear"
	clearWindow_ :: Ptr RawWindow -> IO ()


data RawWindow

-- | Window Handle
type Window = ForeignPtr RawWindow


-- | Perform an IO action within an initialized terminal.
withTerm :: IO a -> IO a
withTerm = bracket_ initTerm disposeTerm

-- | The the terminal size.
termSize :: IO (CInt, CInt)
termSize = do
	w <- termWidth
	h <- termHeight
	return (w, h)

-- | Create a new window using a location and size.
newWindow :: (CInt, CInt) -> (CInt, CInt) -> IO Window
newWindow (x, y) (w, h) = newWindow_ h w y x >>= newForeignPtr deleteWindow_


instance Box (ForeignPtr RawWindow) where
	getBounds win = withForeignPtr win $ \t -> do
		x <- windowX t
		y <- windowY t
		w <- windowMaxX t
		h <- windowMaxY t
		return (x, y, w + 1, h + 1)

	setBounds win (x, y, w, h) = withForeignPtr win $ \t -> do
		resizeWindow_ t 1 1
		moveWindow_ t y x
		resizeWindow_ t h w

instance Render (ForeignPtr RawWindow) where
	render win = withForeignPtr win refreshWindow_

instance Canvas (ForeignPtr RawWindow) where
	setCursor win (x, y) = withForeignPtr win (\t -> moveCursor_ t y x)

	getCursor win = withForeignPtr win $ \t -> do
		x <- windowCursorX t
		y <- windowCursorY t
		return (x, y)

	drawCString win str = withForeignPtr win (\t -> addString_ t str)

	clear win = withForeignPtr win clearWindow_
