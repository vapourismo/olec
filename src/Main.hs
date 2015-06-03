{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad.State.Strict

import Numeric

import Data.Char
import Data.List
import Data.Word
import Data.Metrics

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Graphics.Text.Width

import Olec.Interface
import Olec.Interface.Terminal

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

newtype PrintText = PrintText T.Text

toPlainText :: Int -> T.Text -> PrintText
toPlainText n text = PrintText (fitText n (T.filter isPrint text))

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
	= Text Style PrintText
	| VAlign [Image]
	| HAlign [Image]

imageWidth :: Image -> Int
imageWidth (Text _ (PrintText txt)) = fromIntegral (textWidth txt)
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

writeImage :: Display -> Position -> Image -> IO ()
writeImage (Display lock term) o i =
	bracket_ (waitQSem lock) (signalQSem lock) $ do
		-- Reset all attributes and colors
		write "\ESC[m"
		writeForeground initForeground
		writeBackground initBackground

		-- Evaluate the state transformer
		evalStateT (render o i) (Style initForeground initBackground)

	where
		initForeground :: Color
		initForeground = Color 255 255 255

		initBackground :: Color
		initBackground = Color 0 0 0

		write :: B.ByteString -> IO ()
		write = terminalFeed term

		writeMoveCursor :: Position -> IO ()
		writeMoveCursor (x, y) =
			write (B.concat ["\ESC[",
			                 BC.pack (show (y + 1)), ";",
			                 BC.pack (show (x + 1)), "H"])

		writeForeground :: Color -> IO ()
		writeForeground (Color r g b) =
			write (B.concat ["\ESC[38;2;",
			                 BC.pack (show r), ";",
			                 BC.pack (show g), ";",
			                 BC.pack (show b), "m"])

		writeBackground :: Color -> IO ()
		writeBackground (Color r g b) =
			write (B.concat ["\ESC[48;2;",
			                 BC.pack (show r), ";",
			                 BC.pack (show g), ";",
			                 BC.pack (show b), "m"])

		writeText :: T.Text -> IO ()
		writeText text =
			write (T.encodeUtf8 text)

		applyStyle :: Style -> StateT Style IO ()
		applyStyle style@(Style fg bg) = do
			Style fg' bg' <- get
			liftIO $ do
				when (fg' /= fg) (writeForeground fg)
				when (bg' /= bg) (writeBackground bg)
			put style

		render :: Position -> Image -> StateT Style IO ()
		render origin img =
			case img of
				Text style (PrintText text) -> do
					applyStyle style
					liftIO $ do
						writeMoveCursor origin
						writeText text

				VAlign imgs ->
					renderVertically origin imgs

				HAlign imgs ->
					renderHorizontally origin imgs

		renderVertically :: Position -> [Image] -> StateT Style IO ()
		renderVertically _ [] = pure ()
		renderVertically origin@(x, y) (img : imgs) = do
			render origin img
			renderVertically (x, y + imageHeight img) imgs

		renderHorizontally :: Position -> [Image] -> StateT Style IO ()
		renderHorizontally _ [] = pure ()
		renderHorizontally origin@(x, y) (img : imgs) = do
			render origin img
			renderHorizontally (x + imageWidth img, y) imgs


main :: IO ()
main = do
	d <- launchUI
	writeImage d (0, 0) (drawText (Style (Color 255 255 255) (Color 0 0 0)) "Hello World" (10, 10))
	threadDelay 5000000
