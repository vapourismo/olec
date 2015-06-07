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
	translate,
	layered,
	center,
	text,
	string,
	vcat,
	vbox,
	vbox',
	hcat,
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
	| VCat [Image]
	| HCat [Image]
	| Maximized Int Int Image
	| Translated Int Int Image
	| Layered [Image]
	| Empty

-- | How many columns does the "Image" need?
imageWidth :: Image -> Int
imageWidth (Empty) = 0
imageWidth (Maximized w _ img) = max w (imageWidth img)
imageWidth (Translated x _ img) = x + imageWidth img
imageWidth (Layered imgs) = foldl' (\ w i -> max w (imageWidth i)) 0 imgs
imageWidth (Text _ txt) = textWidth txt
imageWidth (VCat imgs) = foldl' (\ w i -> max w (imageWidth i)) 0 imgs
imageWidth (HCat imgs) = sum (map imageWidth imgs)

-- | How many rows does the "Image" need?
imageHeight :: Image -> Int
imageHeight (Empty) = 0
imageHeight (Maximized _ h img) = max h (imageHeight img)
imageHeight (Translated _ y img) = y + imageHeight img
imageHeight (Layered imgs) = foldl' (\ w i -> max w (imageHeight i)) 0 imgs
imageHeight (Text _ _) = 1
imageHeight (VCat imgs) = sum (map imageHeight imgs)
imageHeight (HCat imgs) = foldl' (\ w i -> max w (imageHeight i)) 0 imgs

-- | An "Image" producer
type Painter = ReaderT Size IO Image

class Paintable a where
	toPainter :: a -> Painter

-- | Generate the "Image"
paintImage :: Painter -> Size -> IO Image
paintImage = runReaderT

-- | Extend the produced "Image" width and height to match the requested width and height.
maximize :: Painter -> Painter
maximize painter =
	uncurry Maximized <$> ask <*> painter

-- | Move the produced "Image" within the borders of this "Painter".
translate :: Int -> Int -> Painter -> Painter
translate x y painter = do
	(w, h) <- ask
	img <- painter

	let x' =
		if imageWidth img > w then
			0
		else
			min (w - imageWidth img) x

	let y' =
		if imageHeight img > h then
			0
		else
			min (h - imageHeight img) y

	if x' <= 0 && y' <= 0 then
		pure img
	else
		pure (Translated x' y' img)

-- | Layered "Image"
layered :: [Painter] -> Painter
layered ps = Layered <$> sequence ps

-- | Center the produced "Image" on the canvas.
center :: Painter -> Painter
center painter = do
	(w, h) <- ask
	img <- painter
	translate ((w - imageWidth img) `div` 2) ((h - imageHeight img) `div` 2) (pure img)

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

-- | Concat "Painter"s vertically.
vcat :: [Painter] -> Painter
vcat ps = VCat <$> sequence ps

-- | Align many "Painter"s vertically.
vbox :: [DivisionHint Int Float Painter] -> Painter
vbox hints = do
	(_, height) <- ask
	VCat <$> sequence (make 0 (divideMetric hints height))
	where
		make :: Int -> [(Int, Painter)] -> [Painter]
		make _ [] = []
		make offset ((rheight, p) : cs) =
			local (\ (width, _) -> (width, rheight)) p : make (offset + rheight) cs

-- | Similiar to "vbox", but allows "Painter"s to use less space then delegated.
vbox' :: [DivisionHint Int Float Painter] -> Painter
vbox' hints = do
	(_, height) <- ask
	VCat . snd <$> divideMetricFitted hints height constrain
	where
		constrain :: (Int, Painter) -> ReaderT Size IO (Int, Image)
		constrain (rheight, visualizer) =
			if rheight > 0 then
				fmap (\ img -> (imageHeight img, img))
				     (local (\ (width, _) -> (width, rheight)) visualizer)
			else
				pure (0, Empty)

-- | Concat "Painter"s horizontally.
hcat :: [Painter] -> Painter
hcat ps = HCat <$> sequence ps

-- | Align many "Painter"s horizontally.
hbox :: [DivisionHint Int Float Painter] -> Painter
hbox hints = do
	(width, _) <- ask
	VCat <$> sequence (make 0 (divideMetric hints width))
	where
		make :: Int -> [(Int, Painter)] -> [Painter]
		make _ [] = []
		make offset ((rwidth, p) : cs) =
			local (\ (_, height) -> (rwidth, height)) p : make (offset + rwidth) cs

-- | Similiar to "hbox", but allows "Painter"s to use less space then delegated.
hbox' :: [DivisionHint Int Float Painter] -> Painter
hbox' hints = do
	(width, _) <- ask
	HCat . snd <$> divideMetricFitted hints width constrain
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

		Translated x y sub ->
			renderOne out (let (x0, y0) = origin in (x0 + x, y0 + y)) sub

		Layered sub ->
			mapM_ (renderOne out origin) sub

		Text style txt -> do
			changeStyle out style
			liftIO $ do
				moveCursor out origin
				drawText out txt

		VCat imgs ->
			renderVertically out origin imgs

		HCat imgs ->
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
