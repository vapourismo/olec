{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Olec.Interface.Renderer (
	-- * Canvas
	Canvas (..),

	-- * Type-related utilities
	textWidth,
	fitText,
	stringWidth,
	fitString,

	-- * Renderer
	Renderer,
	Visual (..),
	runRenderer,

	-- * Utilities
	constrainRenderer,
	getSize,
	resetCursor,
	moveCursor,

	-- * Text
	drawText,
	drawString,

	-- * Auxiliary
	fillDrawingArea,
	resetStyle,

	-- * Colors
	Color (..),
	setForegroundColor,
	resetForegroundColor,
	setBackgroundColor,
	resetBackgroundColor,

	-- * Attributes
) where

import Control.Monad
import Control.Monad.RWS.Strict

import Numeric

import Data.List
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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
writeCursorPosition x y =
	tell (B.concat ["\ESC[",
	                BC.pack (show (y + 1)),
	                ";",
	                BC.pack (show (x + 1)),
	                "H"])

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

-- | Types which can be drawn upon
class Canvas a where
	canvasSize   :: a -> IO Size

	canvasOrigin :: a -> IO Position

	feedCanvas   :: a -> B.ByteString -> IO ()

-- | Render something in a constrained area.
newtype Renderer a = Renderer { unwrapRenderer :: RWST Info B.ByteString Position IO a }
	deriving (Functor, Applicative, Monad, MonadReader Info,
	          MonadState Position, MonadWriter B.ByteString,
	          MonadIO)

-- | Types which have a visual representation
class Visual a where
	visualize :: a -> Renderer ()

-- | Execute the rendering actions.
runRenderer :: (Canvas c) => Renderer a -> c -> IO a
runRenderer renderer canvas = do
	origin <- canvasOrigin canvas
	size <- canvasSize canvas

	(result, _, msg) <- runRWST (unwrapRenderer (uncurry writeCursorPosition origin
	                                             >> renderer))
	                            (Info origin size)
	                            (0, 0)

	feedCanvas canvas msg
	pure result

-- | Constrain a "Renderer" to an area.
constrainRenderer :: Position -> Size -> Renderer a -> Renderer a
constrainRenderer (x, y) (rw, rh) (Renderer renderer) =
	Renderer $ withRWST transform $ do
		unwrapRenderer resetCursor
		renderer
	where
		transform (Info (x0, y0) (w, h)) _ =
			let
				origin = (x0 + min (w - 1) (max 0 x),
				          y0 + min (h - 1) (max 0 y))
				size = (min (w - min (w - 1) (max 0 x)) rw,
				        min (h - min (h - 1) (max 0 y)) rh)
			in
				(Info origin size, (0, 0))

-- | Get the size of the current drawing area.
getSize :: Renderer Size
getSize = asks (\ (Info _ size) -> size)

-- | Reset the cursor position to its origin.
resetCursor :: Renderer ()
resetCursor = do
	Info (x0, y0) _ <- ask
	writeCursorPosition x0 y0
	put (0, 0)

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

	tell (T.encodeUtf8 (fitText (w - (x - x0)) txt))

-- | Draw a "String" at the current position.
drawString :: String -> Renderer ()
drawString str = do
	Info (x0, _) (w, _) <- ask
	(x, _) <- get

	tell (UB.fromString (fitString (w - (x - x0)) str))

-- | Fill the area with a given character.
fillDrawingArea :: Char -> Renderer ()
fillDrawingArea c =
	when (safeWcwidth c > 0) $ do
		(w, h) <- getSize

		let lineWidth = div w (safeWcwidth c)
		let line =
			if lineWidth < w then
				replicate lineWidth c ++ replicate (w - lineWidth) ' '
			else
				replicate lineWidth c

		forM_ [0 .. h - 1] $ \ y -> do
			moveCursor 0 y
			drawString line

-- | Reset all colors and attributes.
resetStyle :: Renderer ()
resetStyle = tell "\ESC[m"

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
--   TODO: Check if color is already present.
setForegroundColor :: Color -> Renderer ()
setForegroundColor (Color r g b) =
	tell (B.concat ["\ESC[38;2;",
	                BC.pack (show r),
	                ";",
	                BC.pack (show g),
	                ";",
	                BC.pack (show b),
	                "m"])

-- | Reset the current foreground color.
resetForegroundColor :: Renderer ()
resetForegroundColor =
	tell "\ESC[30m"

-- | Adjust the background color.
--   TODO: Check if color is already present.
setBackgroundColor :: Color -> Renderer ()
setBackgroundColor (Color r g b) =
	tell (B.concat ["\ESC[48;2;",
	                BC.pack (show r),
	                ";",
	                BC.pack (show g),
	                ";",
	                BC.pack (show b),
	                "m"])

-- | Reset the current background color.
resetBackgroundColor :: Renderer ()
resetBackgroundColor =
	tell "\ESC[40m"
