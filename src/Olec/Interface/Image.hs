{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface.Image (
	-- * Image
	Image,
	imageWidth,
	imageHeight,

	-- * Visualisers
	Visualiser,
	runVisualizer,
	Visual (..),
	drawText,
	drawString,
	alignVertically,
	alignHorizontally,

	-- * Output
	Output (..),
	outputImage
) where

import Control.Exception

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Metrics
import Data.Foldable

import Data.Char

import qualified Data.Text as T

import Graphics.Text.Width

import Olec.Interface.Types

-- | How many columns does the "Text" occupy?
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
type Visualiser = ReaderT Size IO Image

-- | Generate the "Image"
runVisualizer :: Size -> Visualiser -> IO Image
runVisualizer = flip runReaderT

class Visual a where
	visualize :: a -> Visualiser

-- | Draw "Text" in a given "Style".
drawText :: Style -> T.Text -> Visualiser
drawText style text = do
	(w, h) <- ask
	pure $
		if h >= 1 then
			Text style (fitText w (T.filter isPrint text))
		else
			Empty

-- | Draw "String" in a given "Style".
drawString :: Style -> String -> Visualiser
drawString style text = do
	(w, h) <- ask
	pure $
		if h >= 1 then
			Text style (T.pack (fitString w (filter isPrint text)))
		else
			Empty

-- | Align many "Visualiser"s vertically.
alignVertically :: [DivisionHint Int Float Visualiser] -> Visualiser
alignVertically hints = do
	(_, height) <- ask
	VAlign . snd <$> divideMetricFitted hints height constrain
	where
		constrain :: (Int, Visualiser) -> ReaderT Size IO (Int, Image)
		constrain (rheight, visualizer) = do
			if rheight > 0 then
				fmap (\ img -> (imageHeight img, img))
				     (local (\ (width, _) -> (width, rheight)) visualizer)
			else
				pure (0, Empty)

-- | Align many "Visualiser"s horizontally.
alignHorizontally :: [DivisionHint Int Float Visualiser] -> Visualiser
alignHorizontally hints = do
	(width, _) <- ask
	HAlign . snd <$> divideMetricFitted hints width constrain
	where
		constrain :: (Int, Visualiser) -> ReaderT Size IO (Int, Image)
		constrain (rwidth, visualizer) = do
			if rwidth > 0 then
				fmap (\ img -> (imageWidth img, img))
				     (local (\ (_, height) -> (rwidth, height)) visualizer)
			else
				pure (0, Empty)

-- | VT100-style output
class Output a where
	lockOutput :: a -> IO ()

	unlockOutput :: a -> IO ()

	sizeOfOutput :: a -> IO Size

	clearOutput :: a -> IO ()

	setForeground :: a -> Color -> IO ()

	setBackground :: a -> Color -> IO ()

	writeText :: a -> T.Text -> IO ()

	moveCursor :: a -> Position -> IO ()

-- | Output the "Image" at a given "Position".
outputImage :: (Output o) => o -> Position -> Image -> IO ()
outputImage out origin img =
	bracket_ (lockOutput out)
	         (unlockOutput out)
	         (evalStateT (renderOne out origin img) Nothing)

-- | Image output
type ImageOutput = StateT (Maybe Style) IO

-- | Change the "Style".
changeStyle :: (Output o) => o -> Style -> ImageOutput ()
changeStyle out style@(Style fg bg) = do
	mbStyle <- get
	case mbStyle of
		Nothing ->
			liftIO $ do
				setForeground out fg
				setBackground out bg

		Just (Style fg' bg') ->
			liftIO $ do
				when (fg' /= fg) (setForeground out fg)
				when (bg' /= bg) (setBackground out bg)

	put (Just style)

-- | Render one "Image".
renderOne :: (Output o) => o -> Position -> Image -> ImageOutput ()
renderOne out origin img =
	case img of
		Empty -> pure ()

		Text style text -> do
			changeStyle out style
			liftIO $ do
				moveCursor out origin
				writeText out text

		VAlign imgs ->
			renderVertically out origin imgs

		HAlign imgs ->
			renderHorizontally out origin imgs

-- | Render some "Image"s vertically aligned.
renderVertically :: (Output o) => o -> Position -> [Image] -> ImageOutput ()
renderVertically _ _ [] = pure ()
renderVertically out origin@(x, y) (img : imgs) = do
	renderOne out origin img
	renderVertically out (x, y + imageHeight img) imgs

-- | Render some "Images"s horizontally aligned.
renderHorizontally :: (Output o) => o -> Position -> [Image] -> ImageOutput ()
renderHorizontally _ _ [] = pure ()
renderHorizontally out origin@(x, y) (img : imgs) = do
	renderOne out origin img
	renderHorizontally out (x + imageWidth img, y) imgs
