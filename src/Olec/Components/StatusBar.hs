{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Olec.Components.StatusBar (
	StatusBar (..),
	sbLeftText, sbTime, sbStyle,
	renderStatusBar,
	runStatusBar
) where

import Control.Lens
import Control.Monad
import Control.Concurrent

import Data.Time
import Data.Default
import qualified Data.Text as T

import Olec.Runtime
import Olec.Components.Layouts

-- | Status Bar
data StatusBar = StatusBar {
	_sbLeftText  :: T.Text,
	_sbTime      :: UTCTime,
	_sbStyle     :: Attr
}

makeLenses ''StatusBar

instance Default StatusBar where
	def = StatusBar "" (UTCTime (toEnum 0) (secondsToDiffTime 0)) (mkAttr 0 7)

-- | Renderer for "StatusBar"
renderStatusBar :: Renderer StatusBar
renderStatusBar = do
	StatusBar {..} <- getRenderState

	let timeString = formatTime defaultTimeLocale "%T" _sbTime

	let space = fillChar _sbStyle ' '
	alignHorizontally
		[
			Absolute 1 space,
			LeftOver (justifyLeft space (drawText _sbStyle _sbLeftText)),
			Absolute 1 space,
			LeftOver (justifyRight space (drawString _sbStyle timeString)),
			Absolute 1 space
		]

-- |
runStatusBar :: Runtime StatusBar ()
runStatusBar = forever $ do
	time <- liftIO (threadDelay 1000000 >> getCurrentTime)
	sbTime .= time
