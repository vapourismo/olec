{-# LANGUAGE TemplateHaskell #-}

module Olec.Components.StatusBar (
	StatusBar (..),
	sbLeftText, sbRightText, sbStyle,
	renderStatusBar,
	runStatusBar
) where

import Control.Lens
import Control.Monad
import Control.Concurrent

import qualified Data.Text as T

import Olec.Runtime
import Olec.Components.Layouts

-- | Status Bar
data StatusBar = StatusBar {
	_sbLeftText  :: T.Text,
	_sbRightText :: T.Text,
	_sbStyle     :: Attr
}

$(makeLenses ''StatusBar)

-- | Renderer for "StatusBar"
renderStatusBar :: Renderer StatusBar
renderStatusBar = do
	StatusBar left right style <- getRenderState
	let space = fillChar style ' '
	alignHorizontally
		[
			Absolute 1 space,
			LeftOver (justifyLeft space (drawText style left)),
			Absolute 1 space,
			LeftOver (justifyRight space (drawText style right)),
			Absolute 1 space
		]

-- |
runStatusBar :: Runtime StatusBar ()
runStatusBar = forever $ do
	liftIO (threadDelay 250000)
	sbLeftText %= maybe T.empty (uncurry (flip T.snoc)) . T.uncons
	render
