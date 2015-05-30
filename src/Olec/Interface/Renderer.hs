{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface.Renderer (
	-- * Type-related utilities
	textWidth,
	stringWidth,

	-- * Renderer
	Renderer,
	runRenderer,

	-- * Utilities
	constrainRenderer,
	flush,
	getSize,
	resetCursor,
	moveCursor,

	-- * Text
	drawText,
	drawString,

	-- * Colors
	Color (..),
	setForegroundColor,
	resetForegroundColor,
	setBackgroundColor,
	resetBackgroundColor,

	-- * Attributes
) where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Numeric

import Data.List
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Graphics.Text.Width

import System.IO

import Olec.Interface.Types

-- | Get the number of columns needed to display the given "Text".
textWidth :: T.Text -> Int
textWidth = T.foldl' (\ n c -> n + safeWcwidth c) 0

-- | Get the number of columns needed to display the given "String".
stringWidth :: String -> Int
stringWidth = wcswidth

-- | Set the cursor position.
writeCursorPosition :: Handle -> Int -> Int -> IO ()
writeCursorPosition h x y = do
	B.hPut h "\ESC["
	hPutStr h (show (y + 1))
	B.hPut h ";"
	hPutStr h (show (x + 1))
	B.hPut h "H"

-- | Write a RGB triple.
writeRGB :: Handle -> Word8 -> Word8 -> Word8 -> IO ()
writeRGB h r g b = do
	hPutStr h (show r)
	B.hPut h ";"
	hPutStr h (show g)
	B.hPut h ";"
	hPutStr h (show b)

-- | Reset the foreground color.
writeResetForegroundColor :: Handle -> IO ()
writeResetForegroundColor h =
	B.hPut h "\ESC[30m"

-- | Set the foreground color.
writeForegroundColor :: Handle -> Word8 -> Word8 -> Word8 -> IO ()
writeForegroundColor h r g b = do
	B.hPut h "\ESC[38;2;"
	writeRGB h r g b
	B.hPut h "m"

-- | Reset the background color.
writeResetBackgroundColor :: Handle -> IO ()
writeResetBackgroundColor h =
	B.hPut h "\ESC[40m"

-- | Set the background color.
writeBackgroundColor :: Handle -> Word8 -> Word8 -> Word8 -> IO ()
writeBackgroundColor h r g b = do
	B.hPut h "\ESC[48;2;"
	writeRGB h r g b
	B.hPut h "m"

-- | Write Text.
writeText :: Handle -> T.Text -> IO ()
writeText h t = B.hPut h (T.encodeUtf8 t)

-- | Write String.
writeString :: Handle -> String -> IO ()
writeString = hPutStr

-- | Info for rendering purposes
data Info = Info Handle Position Size

-- | Shorten the given "Text" to fit in a number of columns.
fitText :: Int -> T.Text -> T.Text
fitText n txt =
	snd (T.foldl' runner (n, T.empty) txt)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, T.snoc acc c)
			| otherwise = (m, acc)

-- | Shorten the given "String" to fit in a number of columns.
fitString :: Int -> String -> String
fitString n str =
	snd (foldl' runner (n, []) str)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, acc ++ [c])
			| otherwise = (m, acc)

-- | Render something in a constrained area.
type Renderer = ReaderT Info (StateT Position IO)

-- | Execute the rendering actions.
runRenderer :: Renderer a -> Handle -> Size -> IO a
runRenderer renderer out size = do
	writeCursorPosition out 0 0
	evalStateT (runReaderT renderer (Info out (0, 0) size)) (0, 0)

-- | Constrain a "Renderer" to an area.
constrainRenderer :: Position -> Size -> Renderer a -> Renderer a
constrainRenderer (x, y) (rw, rh) renderer =
	withReaderT transform (resetCursor >> renderer)
	where
		transform (Info out (x0, y0) (w, h)) =
			let
				origin = (x0 + min (w - 1) (max 0 x),
				          y0 + min (h - 1) (max 0 y))
				size = (min (w - min (w - 1) (max 0 x)) rw,
				        min (h - min (h - 1) (max 0 y)) rh)
			in
				Info out origin size

-- | Render actions immediately.
flush :: Renderer ()
flush = do
	Info out _ _ <- ask
	liftIO (hFlush out)

-- | Get the size of the current drawing area.
getSize :: Renderer Size
getSize = asks (\ (Info _ _ size) -> size)

-- | Reset the cursor position to its origin.
resetCursor :: Renderer ()
resetCursor = do
	Info out origin@(x0, y0) _ <- ask
	liftIO (writeCursorPosition out x0 y0)
	put origin

-- | Move the cursor.
moveCursor :: Int -> Int -> Renderer ()
moveCursor x y = do
	-- Acquire information
	Info out (x0, y0) (w, h) <- ask
	pos <- get

	-- Calculate new cursor position
	let newPos@(absX, absY) = (x0 + min (w - 1) (max 0 x),
	                           y0 + min (h - 1) (max 0 y))

	-- Move cursor if new position differs from old one
	when (newPos /= pos) $ do
		liftIO (writeCursorPosition out absX absY)
		put newPos

-- | Draw a "Text" at the current position.
drawText :: T.Text -> Renderer ()
drawText txt = do
	Info out (x0, _) (w, _) <- ask
	(x, _) <- get

	liftIO (writeText out (fitText (w - (x - x0)) txt))

-- | Draw a "String" at the current position.
drawString :: String -> Renderer ()
drawString str = do
	Info out (x0, _) (w, _) <- ask
	(x, _) <- get

	liftIO (writeString out (fitString (w - (x - x0)) str))

-- | RGB Color
data Color = Color Word8 Word8 Word8
	deriving (Eq, Ord)

instance Show Color where
	show (Color r g b) =
		"#" ++ extendTo2 (showHex r [])
		    ++ extendTo2 (showHex g [])
		    ++ extendTo2 (showHex b [])
		where
			extendTo2 [x] = '0' : [x]
			extendTo2 xs = xs

-- | Adjust the foreground color.
setForegroundColor :: Color -> Renderer ()
setForegroundColor (Color r g b) = do
	Info out _ _ <- ask
	liftIO (writeForegroundColor out r g b)

-- | Reset the current foreground color.
resetForegroundColor :: Renderer ()
resetForegroundColor = do
	Info out _ _ <- ask
	liftIO (writeResetForegroundColor out)

-- | Adjust the background color.
setBackgroundColor :: Color -> Renderer ()
setBackgroundColor (Color r g b) = do
	Info out _ _ <- ask
	liftIO (writeBackgroundColor out r g b)

-- | Reset the current background color.
resetBackgroundColor :: Renderer ()
resetBackgroundColor = do
	Info out _ _ <- ask
	liftIO (writeResetBackgroundColor out)
