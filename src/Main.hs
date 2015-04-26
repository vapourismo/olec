{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main where

import Control.Monad
import Olec.Runtime

-- | Entry point
main :: IO ()
main =
	evalRuntime_ (render >> forever fetchEvent) () renderer
	where
		renderer =
			alignVertically [
				Relative 0.5 (fillChar mempty 'A'),
				LeftOver (fillChar mempty 'B')
			]
