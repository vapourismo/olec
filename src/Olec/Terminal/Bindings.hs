{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal.Bindings (
	-- * Types
	Window,

	-- * Basics
	withTerm,
	termWidth,
	termHeight,
	termSize,

	-- * Window Interaction
	newWindow,
	moveWindow,
	resizeWindow,

	-- Window Properties
	windowOrigin,
	windowSize,

	-- * Content Modification
	addString,
	addCString,
	refreshWindow,

	-- * Cursor
	moveCursor,
	getCursor
) where

import Control.Exception

--import qualified Data.ByteString as B

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr


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

-- | Window Properties
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

-- | Move a window to a different location.
moveWindow :: Window -> (CInt, CInt) -> IO ()
moveWindow win (x, y) = withForeignPtr win (\h -> moveWindow_ h y x)

-- | Change the size of a window.
resizeWindow :: Window -> (CInt, CInt) -> IO ()
resizeWindow win (w, h) = withForeignPtr win (\t -> resizeWindow_ t h w)

-- | Add a string to a window.
addString :: Window -> String -> IO ()
addString win str = withForeignPtr win (withCString str . addString_)

-- | Add a C native string to a window.
addCString :: Window -> CString -> IO ()
addCString win str = withForeignPtr win (\t -> addString_ t str)

-- | Refresh the contents so the changes will be visible in the terminal.
refreshWindow :: Window -> IO ()
refreshWindow win = withForeignPtr win refreshWindow_

-- | Get the window origin.
windowOrigin :: Window -> IO (CInt, CInt)
windowOrigin win = withForeignPtr win $ \t -> do
	x <- windowX t
	y <- windowY t
	return (x, y)

-- | Get the window size.
windowSize :: Window -> IO (CInt, CInt)
windowSize win = withForeignPtr win $ \t -> do
	x <- windowMaxX t
	y <- windowMaxY t
	return (x + 1, y + 1)

-- | Move the cursor to a specified position.
moveCursor :: Window -> (CInt, CInt) -> IO ()
moveCursor win (x, y) = withForeignPtr win (\t -> moveCursor_ t y x)

-- | Get the cursor position within the window.
getCursor :: Window -> IO (CInt, CInt)
getCursor win = withForeignPtr win $ \t -> do
	x <- windowCursorX t
	y <- windowCursorY t
	return (x, y)
