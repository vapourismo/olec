{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict

import Numeric

import Data.Char
import Data.List
import Data.Word
import Data.Metrics

import qualified Data.Text as T

import Graphics.Text.Width

type Size = (Int, Int)

type Position = (Int, Int)

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

data Style = Style {
	styleForeground :: Color,
	styleBackground :: Color
}

newtype PlainText = PlainText T.Text

toPlainText :: Int -> T.Text -> PlainText
toPlainText n text = PlainText (fitText n (T.filter isPrint text))

textWidth :: T.Text -> Int
textWidth = T.foldl' (\ n c -> n + safeWcwidth c) 0

fitText :: Int -> T.Text -> T.Text
fitText n txt | textWidth txt > n =
	snd (T.foldl' runner (n, T.empty) txt)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, T.snoc acc c)
			| otherwise = (m, acc)
fitText _ txt = txt

data Image
	= Text Style PlainText
	| VAlign [Image]
	| HAlign [Image]

imageWidth :: Image -> Int
imageWidth (Text _ (PlainText txt)) = fromIntegral (textWidth txt)
imageWidth (VAlign imgs) = foldl' (\ w i -> max w (imageWidth i)) 0 imgs
imageWidth (HAlign imgs) = sum (map imageWidth imgs)

imageHeight :: Image -> Int
imageHeight (Text _ _) = 1
imageHeight (VAlign imgs) = sum (map imageHeight imgs)
imageHeight (HAlign imgs) = foldl' (\ w i -> max w (imageHeight i)) 0 imgs

type Visualiser = Size -> Image

drawText :: Style -> T.Text -> Visualiser
drawText style text (w, _) =
	Text style (toPlainText w text)

alignVertically :: [DivisionHint Int Float Visualiser] -> Visualiser
alignVertically hints size@(_, height) =
	VAlign (snd (divideMetricFitted hints height constrain size))
	where
		constrain (rheight, visualizer) (width, _) =
			let img = visualizer (width, rheight)
			in (imageHeight img, img)

class TerminalOutput a where
	-- | Clear the screen
	writeClear      :: a                -> IO ()

	-- | Move the cursor
	writeMoveCursor :: a -> Position    -> IO ()

	-- | Reset all colors
	writeReset      :: a                -> IO ()

	-- | Print text
	writeText       :: a -> T.Text      -> IO ()

	-- | Set foreground color
	writeForeground :: a -> Color       -> IO ()

	-- | Set background color
	writeBackground :: a -> Color       -> IO ()

writeImage :: (TerminalOutput a) => a -> Position -> Image -> IO ()
writeImage output o i = do
	writeReset output
	evalStateT (render o i) (Style (Color 255 255 255) (Color 0 0 0))
	where
		applyStyle :: Style -> StateT Style IO ()
		applyStyle style@(Style fg bg) = do
			Style fg' bg' <- get
			when (fg' /= fg) (liftIO (writeForeground output fg))
			when (bg' /= bg) (liftIO (writeBackground output bg))
			put style

		render :: Position -> Image -> StateT Style IO ()
		render origin img =
			case img of
				Text style (PlainText text) -> do
					applyStyle style
					liftIO $ do
						writeMoveCursor output origin
						writeText output text

				VAlign imgs ->
					renderV origin imgs

				HAlign imgs ->
					renderH origin imgs

		renderV :: Position -> [Image] -> StateT Style IO ()
		renderV _ [] = pure ()
		renderV origin@(x, y) (img : imgs) = do
			render origin img
			renderV (x, y + imageHeight img) imgs

		renderH :: Position -> [Image] -> StateT Style IO ()
		renderH _ [] = pure ()
		renderH origin@(x, y) (img : imgs) = do
			render origin img
			renderH (x + imageWidth img, y) imgs

main :: IO ()
main = pure ()
