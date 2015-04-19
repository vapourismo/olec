module Olec.Render (
	Size,
	Visual (..),

	module Graphics.Vty
) where

import Graphics.Vty hiding (Event)

type Size = (,) Int Int

class Visual a where
	render :: a -> Size -> Image
