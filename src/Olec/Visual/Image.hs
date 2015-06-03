module Olec.Visual.Image (
	-- * Image
	Image (..),
	imageWidth,
	imageHeight,

	-- * Visualisers
	Visualiser,
	drawText,
	drawString,
	alignVertically,
	alignHorizontally,

	-- * Display
	Display (..)
) where

import Data.Metrics
import Data.Foldable

import Data.Char
import qualified Data.Text as T
import Graphics.Text.Width

import Olec.Visual.Types

-- | How many characters does the "Text" occupy?
textWidth :: T.Text -> Int
textWidth = T.foldl' (\ n c -> n + safeWcwidth c) 0

-- | Fit "Text" into a given number of columns.
fitText :: Int -> T.Text -> T.Text
fitText n txt | textWidth txt > n =
	snd (T.foldl' runner (n, T.empty) txt)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, T.snoc acc c)
			| otherwise = (m, acc)
fitText _ txt = txt

-- | Fit "String" into a given number of columns.
fitString :: Int -> String -> String
fitString n txt | safeWcswidth txt > n =
	snd (foldl' runner (n, []) txt)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, acc ++ [c])
			| otherwise = (m, acc)
fitString _ txt = txt

-- | An image
data Image
	= Text Style T.Text
	| VAlign [Image]
	| HAlign [Image]
	| Empty

-- | How many columns does the "Image" need?
imageWidth :: Image -> Int
imageWidth (Empty) = 0
imageWidth (Text _ text) = textWidth text
imageWidth (VAlign imgs) = foldl' (\ w i -> max w (imageWidth i)) 0 imgs
imageWidth (HAlign imgs) = sum (map imageWidth imgs)

-- | How many rows does the "Image" need?
imageHeight :: Image -> Int
imageHeight (Empty) = 0
imageHeight (Text _ _) = 1
imageHeight (VAlign imgs) = sum (map imageHeight imgs)
imageHeight (HAlign imgs) = foldl' (\ w i -> max w (imageHeight i)) 0 imgs

-- | An "Image" generator
type Visualiser = Size -> Image

-- | Draw "Text" in a given "Style".
drawText :: Style -> T.Text -> Visualiser
drawText style text (w, h)
	| h >= 1 = Text style (fitText w (T.filter isPrint text))
	| otherwise = Empty

-- | Draw "String" in a given "Style".
drawString :: Style -> String -> Visualiser
drawString style text (w, h)
	| h >= 1 = Text style (T.pack (fitString w (filter isPrint text)))
	| otherwise = Empty

-- | Align many "Visualiser"s vertically.
alignVertically :: [DivisionHint Int Float Visualiser] -> Visualiser
alignVertically hints size@(_, height) =
	VAlign (snd (divideMetricFitted hints height constrain size))
	where
		constrain (rheight, visualizer) (width, _)
			| rheight > 0 =
				let img = visualizer (width, rheight)
				in (imageHeight img, img)
			| otherwise = (0, Empty)

-- | Align many "Visualiser"s horizontally.
alignHorizontally :: [DivisionHint Int Float Visualiser] -> Visualiser
alignHorizontally hints size@(width, _) =
	HAlign (snd (divideMetricFitted hints width constrain size))
	where
		constrain (rwidth, visualizer) (_, height)
			| rwidth > 0 =
				let img = visualizer (rwidth, height)
				in (imageWidth img, img)
			| otherwise = (0, Empty)

class Display a where
	-- | Retrieve the display size.
	dimensions :: a -> IO Size

	-- | Clear the entire display.
	clear :: a -> IO ()

	-- | Display an "Image" at a given "Position".
	display :: a -> Position -> Image -> IO ()
