module Olec.Components.Layouts (
	justifyRight,
	justifyLeft,
	splitWeighted,
	pad,
	padLeft,
	padRight,
) where

import Olec.Runtime

-- | Justify to the right.
justifyRight :: Renderer a -- ^ Spacer
             -> Renderer a -- ^ Right
             -> Renderer a
justifyRight space renderer = do
	img <- renderer
	alignHorizontally [LeftOver space, Absolute (imageWidth img) (pure img)]

-- | Justify to the left.
justifyLeft :: Renderer a -- ^ Spacer
            -> Renderer a -- ^ Left
            -> Renderer a
justifyLeft space renderer = do
	img <- renderer
	alignHorizontally [Absolute (imageWidth img) (pure img), LeftOver space]

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

-- | Add padding around a canvas.
pad :: Int        -- ^ Width
    -> Renderer a -- ^ Padding
    -> Renderer a -- ^ Contents
    -> Renderer a
pad w space cnts =
	alignHorizontally
		[
			Absolute w space,
			LeftOver cnts,
			Absolute w space
		]

-- | Add padding to the left.
padLeft :: Int        -- ^ Width
        -> Renderer a -- ^ Padding
        -> Renderer a -- ^ Contents
        -> Renderer a
padLeft w space cnts =
	alignHorizontally
		[
			Absolute w space,
			LeftOver cnts
		]

-- | Add padding to the right.
padRight :: Int        -- ^ Width
         -> Renderer a -- ^ Padding
         -> Renderer a -- ^ Contents
         -> Renderer a
padRight w space cnts =
	alignHorizontally
		[
			LeftOver cnts,
			Absolute w space
		]
