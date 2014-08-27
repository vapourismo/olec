{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal (
	-- * Initialization
	withTerm,

	-- * Cursor
	moveCursor,

	-- * Window
	Window,
	newWindow,
	deleteWindow,
	resizeWindow,
	moveWindow,
	resizeWindow,
	refreshWindow,
	windowMaxX,
	windowMaxY,
	windowSize,
	windowX,
	windowY,
	drawCString,
	drawString,
	drawByteString
) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Exception

import qualified Data.ByteString as B


-- | Initialize the terminal.
foreign import ccall unsafe "olec_initterm"
	beginTerm :: IO ()

-- | Finalize the terminal.
foreign import ccall unsafe "endwin"
	endTerm :: IO ()

-- |
foreign import ccall unsafe "newwin"
	newWindow_ :: CInt -> CInt -> CInt -> CInt -> IO (Ptr Window)

-- |
foreign import ccall unsafe "delwin"
	deleteWindow :: Ptr Window -> IO CInt

-- |
foreign import ccall unsafe "mvwin"
	moveWindow_ :: Ptr Window -> CInt -> CInt -> IO CInt

-- |
foreign import ccall unsafe "wresize"
	resizeWindow_ :: Ptr Window -> CInt -> CInt -> IO CInt

-- |
foreign import ccall unsafe "wrefresh"
	refreshWindow :: Ptr Window -> IO CInt

-- |
foreign import ccall unsafe "getmaxx"
	windowMaxX :: Ptr Window -> IO CInt

-- |
foreign import ccall unsafe "getmaxy"
	windowMaxY :: Ptr Window -> IO CInt

-- |
foreign import ccall unsafe "getbegx"
	windowX :: Ptr Window -> IO CInt

-- |
foreign import ccall unsafe "getbegy"
	windowY :: Ptr Window -> IO CInt

-- |
foreign import ccall unsafe "wmove"
	moveCursor :: Ptr Window -> CInt -> CInt -> IO CInt

-- |
foreign import ccall unsafe "waddstr"
	drawCString :: Ptr Window -> CString -> IO CInt


-- | Pseudo Window Type
data Window


-- | Perform actions within an initialized terminal.
--   The terminal will be destroyed afterwards.
withTerm :: IO a -> IO a
withTerm = bracket_ beginTerm endTerm

-- |
newWindow :: (CInt, CInt) -> (CInt, CInt) -> IO (Ptr Window)
newWindow (x, y) (w, h) = newWindow_ h w y x

-- |
resizeWindow :: Ptr Window -> (CInt, CInt) -> IO CInt
resizeWindow win (w, h) = resizeWindow_ win h w

-- |
moveWindow :: Ptr Window -> (CInt, CInt) -> IO CInt
moveWindow win (x, y) = moveWindow_ win y x

-- |
drawByteString :: Ptr Window -> B.ByteString -> IO CInt
drawByteString win bs = B.useAsCString bs (drawCString win)

-- |
drawString :: Ptr Window -> String -> IO CInt
drawString win str = withCString str (drawCString win)

-- |
windowSize :: Ptr Window -> IO (CInt, CInt)
windowSize win = do
	x <- windowMaxX win
	y <- windowMaxY win
	return (x + 1, y + 1)
