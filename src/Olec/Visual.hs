module Olec.Visual (
	-- * Image
	Image,
	imageWidth,
	imageHeight,

	-- * Painter
	Painter,
	Paintable (..),
	paintImage,

	-- * Utilities
	maximize,
	layered,
	text,
	string,
	vbox,
	vbox',
	hbox,
	hbox',

	-- * Canvas
	Canvas (..),
	renderImage,

	-- * Exports
	module Olec.Visual.Types
) where

import Control.Exception

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Metrics
import Data.Foldable

import Data.Char

import qualified Data.Text as T

import Graphics.Text.Width

import Olec.Visual.Types

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
			| otherwise = (-1, acc)
fitText _ txt = txt

-- | Fit "String" into a given number of columns.
fitString :: Int -> String -> String
fitString n txt | safeWcswidth txt > n =
	snd (foldl' runner (n, []) txt)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, acc ++ [c])
			| otherwise = (-1, acc)
fitString _ txt = txt

-- | An image
data Image
	= Text Style T.Text
	| VAlign [Image]
	| HAlign [Image]
	| Maximized Int Int Image
	| Layered [Image]
	| Empty

-- | How many columns does the "Image" need?
imageWidth :: Image -> Int
imageWidth (Empty) = 0
imageWidth (Maximized w _ _) = w
imageWidth (Layered imgs) = foldl' (\ w i -> max w (imageWidth i)) 0 imgs
imageWidth (Text _ txt) = textWidth txt
imageWidth (VAlign imgs) = foldl' (\ w i -> max w (imageWidth i)) 0 imgs
imageWidth (HAlign imgs) = sum (map imageWidth imgs)

-- | How many rows does the "Image" need?
imageHeight :: Image -> Int
imageHeight (Empty) = 0
imageHeight (Maximized _ h _) = h
imageHeight (Layered imgs) = foldl' (\ w i -> max w (imageHeight i)) 0 imgs
imageHeight (Text _ _) = 1
imageHeight (VAlign imgs) = sum (map imageHeight imgs)
imageHeight (HAlign imgs) = foldl' (\ w i -> max w (imageHeight i)) 0 imgs

-- | An "Image" producer
type Painter = ReaderT Size IO Image

class Paintable a where
	toPainter :: a -> Painter

-- | Generate the "Image"
paintImage :: Painter -> Size -> IO Image
paintImage = runReaderT

-- | Extend the produced "Image" width and height to match the requested width and height.
maximize :: Painter -> Painter
maximize painter = do
	(w, h) <- ask
	img <- painter
	pure (Maximized (max w (imageWidth img)) (max h (imageHeight img)) img)

-- | Layered image
layered :: [Painter] -> Painter
layered ps = Layered <$> sequence ps

-- | Draw "Text" in a given "Style".
text :: Style -> T.Text -> Painter
text style txt =
	flip fmap ask $ \ (w, h) ->
		if h >= 1 then
			Text style (fitText w (T.filter isPrint txt))
		else
			Empty

-- | Draw "String" in a given "Style".
string :: Style -> String -> Painter
string style txt =
	flip fmap ask $ \ (w, h) ->
		if h >= 1 then
			Text style (T.pack (fitString w (filter isPrint txt)))
		else
			Empty

-- | Align many "Painter"s vertically.
vbox :: [DivisionHint Int Float Painter] -> Painter
vbox hints = do
	(_, height) <- ask
	VAlign <$> sequence (make 0 (divideMetric hints height))
	where
		make :: Int -> [(Int, Painter)] -> [Painter]
		make _ [] = []
		make offset ((rheight, p) : cs) =
			local (\ (width, _) -> (width, rheight)) p : make (offset + rheight) cs

-- | Similiar to "vbox", but allows "Painter"s to use less space then delegated.
vbox' :: [DivisionHint Int Float Painter] -> Painter
vbox' hints = do
	(_, height) <- ask
	VAlign . snd <$> divideMetricFitted hints height constrain
	where
		constrain :: (Int, Painter) -> ReaderT Size IO (Int, Image)
		constrain (rheight, visualizer) =
			if rheight > 0 then
				fmap (\ img -> (imageHeight img, img))
				     (local (\ (width, _) -> (width, rheight)) visualizer)
			else
				pure (0, Empty)

-- | Align many "Painter"s horizontally.
hbox :: [DivisionHint Int Float Painter] -> Painter
hbox hints = do
	(width, _) <- ask
	VAlign <$> sequence (make 0 (divideMetric hints width))
	where
		make :: Int -> [(Int, Painter)] -> [Painter]
		make _ [] = []
		make offset ((rwidth, p) : cs) =
			local (\ (_, height) -> (rwidth, height)) p : make (offset + rwidth) cs

-- | Similiar to "hbox", but allows "Painter"s to use less space then delegated.
hbox' :: [DivisionHint Int Float Painter] -> Painter
hbox' hints = do
	(width, _) <- ask
	HAlign . snd <$> divideMetricFitted hints width constrain
	where
		constrain :: (Int, Painter) -> ReaderT Size IO (Int, Image)
		constrain (rwidth, visualizer) =
			if rwidth > 0 then
				fmap (\ img -> (imageWidth img, img))
				     (local (\ (_, height) -> (rwidth, height)) visualizer)
			else
				pure (0, Empty)

-- | VT100-style output
class Canvas a where
	lockCanvas :: a -> IO ()

	unlockCanvas :: a -> IO ()

	sizeOfCanvas :: a -> IO Size

	clearCanvas :: a -> IO ()

	hideCursor :: a -> IO ()

	showCursor :: a -> IO ()

	setForeground :: a -> Color -> IO ()

	setBackground :: a -> Color -> IO ()

	drawText :: a -> T.Text -> IO ()

	moveCursor :: a -> Position -> IO ()

-- | Render the "Image" at a given "Position".
renderImage :: (Canvas o) => o -> Position -> Image -> IO ()
renderImage out origin img =
	bracket_ (lockCanvas out)
	         (unlockCanvas out)
	         (evalStateT (renderOne out origin img) Nothing)

-- | Image output
type ImageOutput = StateT (Maybe Style) IO

-- | Change the "Style".
changeStyle :: (Canvas o) => o -> Style -> ImageOutput ()
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
renderOne :: (Canvas o) => o -> Position -> Image -> ImageOutput ()
renderOne out origin img =
	case img of
		Empty ->
			pure ()

		Maximized _ _ sub ->
			renderOne out origin sub

		Layered sub ->
			mapM_ (renderOne out origin) sub

		Text style txt -> do
			changeStyle out style
			liftIO $ do
				moveCursor out origin
				drawText out txt

		VAlign imgs ->
			renderVertically out origin imgs

		HAlign imgs ->
			renderHorizontally out origin imgs

-- | Render some "Image"s vertically aligned.
renderVertically :: (Canvas o) => o -> Position -> [Image] -> ImageOutput ()
renderVertically _ _ [] = pure ()
renderVertically out origin@(x, y) (img : imgs) = do
	renderOne out origin img
	renderVertically out (x, y + imageHeight img) imgs

-- | Render some "Images"s horizontally aligned.
renderHorizontally :: (Canvas o) => o -> Position -> [Image] -> ImageOutput ()
renderHorizontally _ _ [] = pure ()
renderHorizontally out origin@(x, y) (img : imgs) = do
	renderOne out origin img
	renderHorizontally out (x + imageWidth img, y) imgs
