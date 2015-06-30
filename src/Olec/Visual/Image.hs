{-# LANGUAGE FlexibleInstances #-}

module Olec.Visual.Image (
	-- * Auxiliary
	textWidth,

	-- * Image
	Image (..),

	-- * Painter
	Painter,
	Paintable (..),
	paintImage,

	-- * Utilities
	maximize,
	translate,
	layered,
	center,
	justifyRight,
	text,
	string,
	vcat,
	vbox,
	vbox',
	hcat,
	hbox,
	hbox',

	-- * Intermediate representation
	ImageIR (..),
	toImageIR,
) where

import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Data.Char
import Data.Handle
import Data.Metrics
import Data.Foldable

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

-- | Image contents
data Contents
	= Text Style T.Text
	| VCat [Image]
	| HCat [Image]
	| Translate Int Int Contents
	| Layered [Contents]
	| Empty

-- | Image
data Image = Image {
	imageWidth :: Int,
	imageHeight :: Int,
	imageContents :: Contents
}

-- | An "Image" producer
type Painter = ReaderT Size IO Image

class Paintable a where
	toPainter :: a -> Painter

instance (Paintable a, Handle h) => Paintable (h a) where
	toPainter ref = liftIO (readHandle ref) >>= toPainter

-- | Generate the "Image"
paintImage :: Size -> Painter -> IO Image
paintImage = flip runReaderT

-- | Extend the produced "Image" width and height to match the requested width and height.
maximize :: Painter -> Painter
maximize painter =
	uncurry Image <$> ask <*> fmap imageContents painter

-- | Move the produced "Image" within the borders of this "Painter".
translate :: Int -> Int -> Painter -> Painter
translate x y painter = do
	(w, h) <- ask
	img@(Image width height cnts) <- painter

	let x' =
		if width > w then
			0
		else
			min (w - width) x

	let y' =
		if height > h then
			0
		else
			min (h - height) y

	if x' <= 0 && y' <= 0 then
		pure img
	else
		pure (Image (width + x') (height + y') (Translate x' y' cnts))

-- | Layered "Image"
layered :: [Painter] -> Painter
layered ps =
	fmap make (sequence ps)
	where
		make imgs =
			let (w, h) = foldl' (\ (width', height') (Image width height _) ->
			                         (max width' width, max height' height))
			                    (0, 0)
			                    imgs
			in Image w h (Layered (map imageContents imgs))

-- | Center the produced "Image" on the canvas.
center :: Painter -> Painter
center painter = do
	(w, h) <- ask
	img <- painter
	translate ((w - imageWidth img) `div` 2) ((h - imageHeight img) `div` 2) (pure img)

-- | Justify the painted "Image" to the right.
justifyRight :: Painter -> Painter
justifyRight painter = do
	(w, _) <- ask
	img <- painter
	translate (w - imageWidth img) 0 (pure img)

-- | Draw "Text" in a given "Style".
text :: Style -> T.Text -> Painter
text style txt =
	flip fmap ask $ \ (w, h) ->
		if h >= 1 then
			let
				txt' = fitText w (T.filter isPrint txt)
				height = if T.null txt' then 0 else 1
			in Image (textWidth txt') height (Text style txt')
		else
			Image 0 0 Empty

-- | Draw "String" in a given "Style".
string :: Style -> String -> Painter
string style str = text style (T.pack str)

-- | Concat images vertically.
vcatImages :: [Image] -> Image
vcatImages imgs =
	let (w, h) = foldl' (\ (width', height') (Image width height _) ->
	                         (max width' width, height' + height))
	                    (0, 0)
	                    imgs
	in Image w h (VCat imgs)

-- | Concat images horizontally.
hcatImages :: [Image] -> Image
hcatImages imgs =
	let (w, h) = foldl' (\ (width', height') (Image width height _) ->
	                         (width' + width, max height' height))
	                    (0, 0)
	                    imgs
	in Image w h (HCat imgs)

-- | Concat "Painter"s vertically.
vcat :: [Painter] -> Painter
vcat = fmap vcatImages . sequence

-- | Align many "Painter"s vertically.
vbox :: [DivisionHint Int Float Painter] -> Painter
vbox hints = do
	(_, height) <- ask
	vcat (make 0 (divideMetric hints height))
	where
		make :: Int -> [(Int, Painter)] -> [Painter]
		make _ [] = []
		make offset ((rheight, p) : cs) =
			local (second (const rheight)) p : make (offset + rheight) cs

-- | Similiar to "vbox", but allows "Painter"s to use less space then delegated.
vbox' :: [DivisionHint Int Float Painter] -> Painter
vbox' hints = do
	(_, height) <- ask
	vcatImages . snd <$> divideMetricFitted hints height constrain
	where
		constrain :: (Int, Painter) -> ReaderT Size IO (Int, Image)
		constrain (rheight, visualizer) =
			if rheight > 0 then
				fmap (imageHeight &&& id)
				     (local (second (const rheight)) visualizer)
			else
				pure (0, Image 0 0 Empty)

-- | Concat "Painter"s horizontally.
hcat :: [Painter] -> Painter
hcat = fmap hcatImages . sequence

-- | Align many "Painter"s horizontally.
hbox :: [DivisionHint Int Float Painter] -> Painter
hbox hints = do
	(width, _) <- ask
	hcat (make 0 (divideMetric hints width))
	where
		make :: Int -> [(Int, Painter)] -> [Painter]
		make _ [] = []
		make offset ((rwidth, p) : cs) =
			local (first (const rwidth)) p : make (offset + rwidth) cs

-- | Similiar to "hbox", but allows "Painter"s to use less space then delegated.
hbox' :: [DivisionHint Int Float Painter] -> Painter
hbox' hints = do
	(width, _) <- ask
	hcatImages . snd <$> divideMetricFitted hints width constrain
	where
		constrain :: (Int, Painter) -> ReaderT Size IO (Int, Image)
		constrain (rwidth, visualizer) =
			if rwidth > 0 then
				fmap (imageWidth &&& id)
				     (local (first (const rwidth)) visualizer)
			else
				pure (0, Image 0 0 Empty)

-- | Image intermediate representation
class (Monoid a) => ImageIR a where
	mkSetForeground :: Color -> a

	mkSetBackground :: Color -> a

	mkMoveCursor :: Position -> a

	mkText :: T.Text -> a

-- | Render the "Image" at a given "Position".
toImageIR :: (ImageIR a) => Position -> Image -> a
toImageIR origin (Image _ _ cnts) =
	execWriter (runStateT (renderOne origin cnts) Nothing)

-- | Image output
type ImageOutput a = StateT (Maybe Style) (Writer a) ()

-- | Change style.
changeStyle :: (ImageIR a) => Style -> ImageOutput a
changeStyle style@(Style fg bg) = do
	mbStyle <- get
	case mbStyle of
		Nothing ->
			tell (mkSetForeground fg <> mkSetBackground bg)

		Just (Style fg' bg') -> do
			when (fg' /= fg) (tell (mkSetForeground fg))
			when (bg' /= bg) (tell (mkSetBackground bg))

	put (Just style)

-- | Render a single image.
renderOne :: (ImageIR a) => Position -> Contents -> ImageOutput a
renderOne origin cnts =
	case cnts of
		Empty ->
			pure ()

		Translate x y sub ->
			renderOne (let (x0, y0) = origin in (x0 + x, y0 + y)) sub

		Layered sub ->
			mapM_ (renderOne origin) sub

		Text style txt -> do
			changeStyle style
			tell (mkMoveCursor origin)
			tell (mkText txt)

		VCat imgs ->
			renderVertically origin imgs

		HCat imgs ->
			renderHorizontally origin imgs

-- | Render multiple images vertically aligned.
renderVertically :: (ImageIR a) => Position -> [Image] -> ImageOutput a
renderVertically _ [] = pure ()
renderVertically origin@(x, y) (img : imgs) = do
	renderOne origin (imageContents img)
	renderVertically (x, y + imageHeight img) imgs

-- | Render multiple images horizontally aligned.
renderHorizontally :: (ImageIR a) => Position -> [Image] -> ImageOutput a
renderHorizontally _ [] = pure ()
renderHorizontally origin@(x, y) (img : imgs) = do
	renderOne origin (imageContents img)
	renderHorizontally (x + imageWidth img, y) imgs
