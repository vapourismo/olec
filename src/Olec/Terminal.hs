{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal (
	-- * Terminal Interaction
	withTerm,
	termBegin,
	termEnd,

	-- * Properties
	termSize,

	-- * Cursor
	termMoveCursor,
	termGetCursor,

	-- * Drawing
	termDrawString,
	termDrawChar,
	termDrawByteString,
	termClear,
	termRender
) where

import Foreign.C

import Control.Applicative
import Control.Exception

import qualified Data.ByteString as B


-- Bindings for internal use
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
	termBegin :: IO ()

-- | Finalize the terminal.
foreign import ccall unsafe "terminal_end"
	termEnd :: IO ()

-- | Clear the terminal.
foreign import ccall unsafe "terminal_clear"
	termClear :: IO ()

-- | Commit changes to the terminal.
foreign import ccall unsafe "terminal_render"
	termRender :: IO ()


-- | Perform actions within an initialized terminal.
--   The terminal will be destroyed afterwards.
withTerm :: IO a -> IO a
withTerm = bracket_ termBegin termEnd


-- | Get the terminal width and height.
termSize :: IO (Int, Int)
termSize = (,) <$> fmap fromIntegral _termWidth
               <*> fmap fromIntegral _termHeight


-- | Draw a ByteString.
termDrawByteString :: B.ByteString -> IO ()
termDrawByteString bstr = B.useAsCString bstr _termDrawCString

-- | Draw a String.
termDrawString :: String -> IO ()
termDrawString s = withCString s _termDrawCString

-- | Draw a Char.
termDrawChar :: Char -> IO ()
termDrawChar = _termDrawChar . castCharToCChar


-- | Get the cursor's position.
termGetCursor :: IO (Int, Int)
termGetCursor = (,) <$> fmap fromIntegral _termCursorX
                    <*> fmap fromIntegral _termCursorY

-- | Move the cursor to the given position.
termMoveCursor :: (Int, Int) -> IO ()
termMoveCursor (x, y) = _termMoveCursor (fromIntegral x) (fromIntegral y)
