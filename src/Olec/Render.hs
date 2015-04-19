module Olec.Render (
	Size,
	Visual (..),
	render',

	module Graphics.Vty
) where

import Graphics.Vty hiding (Event, Modifier)

type Size = (,) Int Int

class Visual a where
	render :: a -> Size -> Image

-- | Just like "render" but makes sure the give size is not exceeded.
render' :: (Visual a) => a -> Size -> Image
render' vis size@(width, height) = resize width height (render vis size)
