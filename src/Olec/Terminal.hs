{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal (
	-- * Terminal Interaction
	withTerm,
	beginTerm,
	endTerm,

	-- * Properties
	termDimension,

	-- * Cursor
	moveCursor,
	cursor,

	-- * Drawing
	drawString,
	drawChar,
	drawByteString,
	drawByteString8,
	clearTerm,
	clearToEOL,

	-- * Rendering
	render
) where

import Foreign.C
import Foreign.Marshal.Alloc (free)

import Control.Applicative
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- Foreign Imports
foreign import ccall unsafe "terminal_begin"
	beginTerm :: IO ()

foreign import ccall unsafe "terminal_end"
	endTerm :: IO ()

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

-- | Clear the terminal.
foreign import ccall unsafe "terminal_clear"
	clearTerm :: IO ()

-- | Clear the rest of the line
foreign import ccall unsafe "terminal_clear_eol"
	clearToEOL :: IO ()

-- | Commit changes to the terminal.
foreign import ccall unsafe "terminal_render"
	render :: IO ()

-- | Groups terminal width and height.
termDimension :: IO (Int, Int)
termDimension = (,) <$> fmap cint2int _termWidth
                    <*> fmap cint2int _termHeight where
	cint2int = fromInteger . toInteger

-- | Get the current cursor position.
cursor :: IO (Int, Int)
cursor = (,) <$> fmap cint2int _termCursorX
             <*> fmap cint2int _termCursorY where
	cint2int = fromInteger . toInteger

-- | Perform actions within an initialized terminal. The terminal will be destroyed afterwards.
withTerm :: IO a -> IO a
withTerm = bracket_ beginTerm endTerm

-- | Draw a ByteString.
drawByteString :: B.ByteString -> IO ()
drawByteString bstr = B.useAsCString bstr _termDrawCString

-- | Draw a ByteString.Char8.
drawByteString8 :: C.ByteString -> IO ()
drawByteString8 bstr = C.useAsCString bstr _termDrawCString

-- | Draw a string
drawString :: String -> IO ()
drawString s = do
	cstr <- newCString s
	_termDrawCString cstr
	free cstr

-- | Draw a character
drawChar :: Char -> IO ()
drawChar = _termDrawChar . castCharToCChar

-- | Change the cursor's position
moveCursor :: Int -> Int -> IO ()
moveCursor x y = _termMoveCursor (int2cint x) (int2cint y) where
	int2cint = fromInteger . toInteger
