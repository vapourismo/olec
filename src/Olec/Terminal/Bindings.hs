{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal.Bindings (
	withTerminal,
	maxXY,

	drawString,
	drawByteString,
	refresh,

	moveCursor,
	getCursor,
) where

import Control.Exception
import Control.Monad

import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString as B


-- Initialization and finalization
foreign import ccall unsafe "olec_init"
	_init :: IO ()

foreign import ccall unsafe "olec_dispose"
	_dispose :: IO ()

-- Rendering
foreign import ccall unsafe "addstr"
	_addstr :: CString -> IO CInt

foreign import ccall unsafe "refresh"
	_refresh :: IO CInt

-- Screen Area
foreign import ccall unsafe "olec_maxx"
	_olecMaxX :: IO CInt

foreign import ccall unsafe "olec_maxy"
	_olecMaxY :: IO CInt

-- Cursor
foreign import ccall unsafe "move"
	_move :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "olec_cursor_x"
	_olecCursorX :: IO CInt

foreign import ccall unsafe "olec_cursor_y"
	_olecCursorY :: IO CInt


-- |
withTerminal :: IO a -> IO a
withTerminal = bracket_ _init _dispose

-- |
maxXY :: IO (CInt, CInt)
maxXY = liftM2 (,) _olecMaxX _olecMaxY


-- |
drawString :: String -> IO ()
drawString str = void (withCString str _addstr)

-- |
drawByteString :: B.ByteString -> IO ()
drawByteString bs = void (B.useAsCString bs _addstr)

-- |
refresh :: IO ()
refresh = void _refresh


-- |
moveCursor :: (CInt, CInt) -> IO ()
moveCursor (x, y) = void (_move y x)

-- |
getCursor :: IO (CInt, CInt)
getCursor = liftM2 (,) _olecCursorX _olecCursorY
