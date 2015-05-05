module Olec.Components.Layouts (
	justifyRight,
	splitWeighted
) where

import Debug.Trace
import Olec.Runtime

-- | Justify to the right.
justifyRight :: Renderer a -- ^ Spacer
             -> Renderer a -- ^ Right
             -> Renderer a
justifyRight space renderer = do
	img <- renderer
	alignHorizontally [LeftOver space, Absolute (imageWidth img) (pure img)]

-- | Splits the pane horizontally in a manner which weighs
--   the width of both renderers.
splitWeighted :: Renderer a -- ^ Left
              -> Renderer a -- ^ Spacer
              -> Renderer a -- ^ Right
              -> Renderer a
splitWeighted left space right = do
	imgLeft <- left
	imgRight <- right
	(w, _) <- getCanvasSize

	if imageWidth imgLeft + imageWidth imgLeft >= w then do
		let leftWidth = fromIntegral (imageWidth imgLeft)
		              / fromIntegral (imageWidth imgLeft + imageWidth imgRight)
		alignHorizontally
			[
				Relative leftWidth left,
				Relative (1 - leftWidth) right
			]
	else
		alignHorizontally
			[
				Absolute (imageWidth imgLeft) (pure imgLeft),
				LeftOver space,
				Absolute (imageWidth imgRight) (pure imgRight)
			]
