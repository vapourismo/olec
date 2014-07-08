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

	-- * Cursor
	moveCursor,
	cursor,

	-- * Drawing
	drawString,
	drawChar,
	drawByteString,
	clearTerm,
	clearToEOL,
	render
) where

import Foreign.C
import Foreign.Marshal.Alloc

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
	beginTerm :: IO ()

-- | Finalize the terminal.
foreign import ccall unsafe "terminal_end"
	endTerm :: IO ()

-- | Clear the terminal.
foreign import ccall unsafe "terminal_clear"
	clearTerm :: IO ()

-- | Clear the rest of the line.
foreign import ccall unsafe "terminal_clear_eol"
	clearToEOL :: IO ()

-- | Commit changes to the terminal.
foreign import ccall unsafe "terminal_render"
	render :: IO ()


-- | A cow.
type Size = (Int, Int)

-- | A sheep.
type Position = (Int, Int)


-- | Perform actions within an initialized terminal.
--   The terminal will be destroyed afterwards.
withTerm :: IO a -> IO a
withTerm = bracket_ beginTerm endTerm


-- | Get the terminal width and height.
termSize :: IO Size
termSize = (,) <$> fmap cint2int _termWidth
               <*> fmap cint2int _termHeight where
	cint2int = fromInteger . toInteger


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
