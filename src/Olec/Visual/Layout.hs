module Olec.Visual.Layout (
	Layout (..),
) where

import Olec.Visual.Types

class Layout a where
	updateLayout :: a -> Position -> Size -> IO ()
