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
	gMoveCursor,
	gCursor,

	-- * Drawing
	gDrawString,
	gDrawChar,
	gDrawByteString,
	clearTerm,
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
gDrawByteString :: B.ByteString -> IO ()
gDrawByteString bstr = B.useAsCString bstr _termDrawCString

-- | Draw a String.
gDrawString :: String -> IO ()
gDrawString s = do
	cstr <- newCString s
	_termDrawCString cstr
	free cstr

-- | Draw a Char.
gDrawChar :: Char -> IO ()
gDrawChar = _termDrawChar . castCharToCChar


-- | Get the cursor's position.
gCursor :: IO Position
gCursor = (,) <$> fmap cint2int _termCursorX
             <*> fmap cint2int _termCursorY where
	cint2int = fromInteger . toInteger

-- | Move the cursor to the given position.
gMoveCursor :: Int -> Int -> IO ()
gMoveCursor x y = _termMoveCursor (int2cint x) (int2cint y) where
	int2cint = fromInteger . toInteger
