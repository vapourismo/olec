{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Olec.Interface.Renderer (
	-- * Canvas
	Canvas (..),

	-- * Type-related utilities
	textWidth,
	stringWidth,

	-- * Renderer
	Renderer,
	runRenderer,

	-- * Utilities
	constrainRenderer,
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

import Control.Monad.RWS.Strict

import Numeric

import Data.List
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB

import Graphics.Text.Width

import Olec.Interface.Types

-- | Get the number of columns needed to display the given "Text".
textWidth :: T.Text -> Int
textWidth = T.foldl' (\ n c -> n + safeWcwidth c) 0

-- | Get the number of columns needed to display the given "String".
stringWidth :: String -> Int
stringWidth = wcswidth

-- | Set the cursor position.
writeCursorPosition :: Int -> Int -> Renderer ()
writeCursorPosition x y = do
	tell "\ESC["
	tell (UB.fromString (show (y + 1)))
	tell ";"
	tell (UB.fromString (show (x + 1)))
	tell "H"

-- | Write a RGB triple.
writeRGB :: Word8 -> Word8 -> Word8 -> Renderer ()
writeRGB r g b = do
	tell (UB.fromString (show r))
	tell ";"
	tell (UB.fromString (show g))
	tell ";"
	tell (UB.fromString (show b))

-- | Reset the foreground color.
writeResetForegroundColor :: Renderer ()
writeResetForegroundColor =
	tell "\ESC[30m"

-- | Set the foreground color.
writeForegroundColor :: Word8 -> Word8 -> Word8 -> Renderer ()
writeForegroundColor r g b = do
	tell "\ESC[38;2;"
	writeRGB r g b
	tell "m"

-- | Reset the background color.
writeResetBackgroundColor :: Renderer ()
writeResetBackgroundColor =
	tell "\ESC[40m"

-- | Set the background color.
writeBackgroundColor :: Word8 -> Word8 -> Word8 -> Renderer ()
writeBackgroundColor r g b = do
	tell "\ESC[48;2;"
	writeRGB r g b
	tell "m"

-- | Write Text.
writeText :: T.Text -> Renderer ()
writeText t = tell (T.encodeUtf8 t)

-- | Write String.
writeString :: String -> Renderer ()
writeString = tell . UB.fromString

-- | Info for rendering purposes
data Info = Info Position Size

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
type Renderer = RWST Info B.ByteString Position IO

-- | Canvas
class Canvas a where
	canvasSize   :: a -> IO Size

	canvasOrigin :: a -> IO Position

	feedCanvas   :: a -> B.ByteString -> IO ()

-- | Execute the rendering actions.
runRenderer :: (Canvas c) => Renderer a -> c -> IO a
runRenderer renderer canvas = do
	origin <- canvasOrigin canvas
	size <- canvasSize canvas

	(result, _, msg) <- runRWST (uncurry writeCursorPosition origin >> renderer)
	                            (Info origin size) (0, 0)

	feedCanvas canvas msg
	pure result

-- | Constrain a "Renderer" to an area.
constrainRenderer :: Position -> Size -> Renderer a -> Renderer a
constrainRenderer (x, y) (rw, rh) renderer =
	withRWST transform (resetCursor >> renderer)
	where
		transform (Info (x0, y0) (w, h)) s =
			let
				origin = (x0 + min (w - 1) (max 0 x),
				          y0 + min (h - 1) (max 0 y))
				size = (min (w - min (w - 1) (max 0 x)) rw,
				        min (h - min (h - 1) (max 0 y)) rh)
			in
				(Info origin size, s)

-- | Get the size of the current drawing area.
getSize :: Renderer Size
getSize = asks (\ (Info _ size) -> size)

-- | Reset the cursor position to its origin.
resetCursor :: Renderer ()
resetCursor = do
	Info origin@(x0, y0) _ <- ask
	writeCursorPosition x0 y0
	put origin

-- | Move the cursor.
moveCursor :: Int -> Int -> Renderer ()
moveCursor x y = do
	-- Acquire information
	Info (x0, y0) (w, h) <- ask
	pos <- get

	-- Calculate new cursor position
	let newPos@(absX, absY) = (x0 + min (w - 1) (max 0 x),
	                           y0 + min (h - 1) (max 0 y))

	-- Move cursor if new position differs from old one
	when (newPos /= pos) $ do
		writeCursorPosition absX absY
		put newPos

-- | Draw a "Text" at the current position.
drawText :: T.Text -> Renderer ()
drawText txt = do
	Info (x0, _) (w, _) <- ask
	(x, _) <- get

	writeText (fitText (w - (x - x0)) txt)

-- | Draw a "String" at the current position.
drawString :: String -> Renderer ()
drawString str = do
	Info (x0, _) (w, _) <- ask
	(x, _) <- get

	writeString (fitString (w - (x - x0)) str)

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
	Info _ _ <- ask
	writeForegroundColor r g b

-- | Reset the current foreground color.
resetForegroundColor :: Renderer ()
resetForegroundColor =
	writeResetForegroundColor

-- | Adjust the background color.
setBackgroundColor :: Color -> Renderer ()
setBackgroundColor (Color r g b) =
	writeBackgroundColor r g b

-- | Reset the current background color.
resetBackgroundColor :: Renderer ()
resetBackgroundColor =
	writeResetBackgroundColor
