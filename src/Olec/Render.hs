{-# LANGUAGE ExistentialQuantification #-}

module Olec.Render (
	-- * Render
	Size,
	Visual (..),
	render',

	-- * Layouts
	LayoutHint (..),
	VLayout (..),

	-- * Auxiliary
	Raster (..),

	-- * External modules
	module Graphics.Vty
) where

import qualified Data.Text as T

import Graphics.Vty hiding (Event)

-- | Dimension
type Size = (,) Int Int

-- | Visualizable data type
class Visual a where
	render :: a -> Size -> Image

-- | Render and resize image
render' :: (Visual a) => a -> Size -> Image
render' x size@(width, height) =
	resize width height (render x size)

instance Visual () where
	render _ _ = emptyImage

instance Visual T.Text where
	render value _ = (text' mempty value)

-- | How to place an element into a layout
data LayoutHint
	= forall a. Visual a => Absolute Int a
	| forall a. Visual a => Relative Float a
	| forall a. Visual a => LeftOver a

-- | Join elements vertically
newtype VLayout = VLayout [LayoutHint]

-- | Like "Either" with existential quantification
data StageTwo
	= Done Image
	| forall a. Visual a => ToGo a

instance Visual VLayout where
	render (VLayout elems) (width, height) =
		vertCat (reverse (stageOne elems [] 0 height))
		where
			processItem a xs carry numLeftOver availableHeight usedHeight =
				stageOne xs
					(Done (render' a (width, usedHeight)) : carry)
					numLeftOver
					(availableHeight - usedHeight)

			calculateHeights num restHeight =
				(d, d + m) where (d, m) = divMod restHeight num

			fetchDone [] = []
			fetchDone (Done img : xs) = img : fetchDone xs
			fetchDone (_ : xs) = fetchDone xs

			stageOne :: [LayoutHint] -> [StageTwo] -> Int -> Int -> [Image]
			stageOne _ carry _ availableHeight
				| availableHeight <= 0 = fetchDone carry
			stageOne [] carry numLeftOver restHeight =
				uncurry (stageTwo carry numLeftOver) (calculateHeights numLeftOver restHeight)
			stageOne (x : xs) carry numLeftOver availableHeight =
				case x of
					Absolute h a | h > 0 ->
						processItem a xs carry numLeftOver
							availableHeight (min h availableHeight)

					Relative f a | f > 0.0 ->
						processItem a xs carry numLeftOver
							availableHeight (round (min f 1.0 * fromIntegral height))

					LeftOver a ->
						stageOne xs (ToGo a : carry) (numLeftOver + 1) availableHeight

					_ -> stageOne xs carry numLeftOver availableHeight

			stageTwo :: [StageTwo] -> Int -> Int -> Int -> [Image]
			stageTwo [] _ _ _ = []
			stageTwo (x : xs) numLeftOver normHeight lastHeight
				| numLeftOver <= 0 = fetchDone (x : xs)
				| otherwise =
					case x of
						Done img -> img : stageTwo xs numLeftOver normHeight lastHeight
						ToGo vis ->
							render' vis (width, if numLeftOver == 1 then lastHeight else normHeight) :
								stageTwo xs numLeftOver normHeight lastHeight

-- | Fill the entire pane with the given character
data Raster = Raster Char

instance Visual Raster where
	render (Raster c) (width, height) =
		vertCat (replicate height (string mempty (replicate width c)))
