{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal (withTerminal,
                      termDimension,
                      moveCursor,
                      drawString, drawChar,
                      render) where

import Foreign.C
import Foreign.Marshal.Alloc (free)

import Control.Applicative
import Control.Exception

-- Foreign Imports
foreign import ccall unsafe "terminal_begin"       _termBegin      :: IO ()
foreign import ccall unsafe "terminal_end"         _termEnd        :: IO ()
foreign import ccall unsafe "terminal_width"       _termWidth      :: IO CInt
foreign import ccall unsafe "terminal_height"      _termHeight     :: IO CInt
foreign import ccall unsafe "terminal_move_cursor" _termMoveCursor :: CInt -> CInt -> IO ()
foreign import ccall unsafe "terminal_draw_char"   _termDrawChar   :: CChar -> IO ()
foreign import ccall unsafe "terminal_draw_string" _termDrawString :: CString -> IO ()

-- | Commit changes to the screen
foreign import ccall unsafe "terminal_render"      render      :: IO ()


cint2int :: CInt -> Int
cint2int = fromInteger . toInteger

int2cint :: Int -> CInt
int2cint = fromInteger . toInteger


-- | Groups terminal width and height
termDimension :: IO (Int, Int)
termDimension = (,) <$> (fmap cint2int _termWidth)
                    <*> (fmap cint2int _termHeight)

-- | Perform actions within an initialized terminal. The terminal will be destroyed afterwards.
withTerminal :: IO a -> IO a
withTerminal = bracket_ _termBegin _termEnd

-- | Draw a string
drawString :: String -> IO ()
drawString s = do
	cstr <- newCString s
	_termDrawString cstr
	free cstr

-- | Draw a character
drawChar :: Char -> IO ()
drawChar = _termDrawChar . castCharToCChar

-- | Change the cursor's position
moveCursor :: Int -> Int -> IO ()
moveCursor x y = _termMoveCursor (int2cint x) (int2cint y)
