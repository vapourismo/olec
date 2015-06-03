{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Metrics

import qualified Data.Text as T

import Olec.Interface

data StatusBarConfig = StatusBarConfig Color Color

data StatusBarText = StatusBarText T.Text

instance Component StatusBarConfig StatusBarText where
	layoutRT = pure ()

	paintRT = do
		StatusBarConfig fg bg <- ask
		StatusBarText mode <- get

		renderRT $ do
			resetStyle
			divideVert [LeftOver   (space fg bg),
			            Absolute 1 (drawBar fg bg mode)]
			resetCursor

		where
			space fg bg = do
				setForegroundColor fg
				setBackgroundColor bg
				fillDrawingArea ' '

			drawBar fg bg mode = do
				space fg bg
				moveCursor 0 0
				drawMode mode

			drawMode mode = do
				setForegroundColor (Color 255 0 0)
				setBackgroundColor (Color 0 0 0)

				drawString " "
				drawText mode
				drawString " "

instance RootComponent StatusBarConfig StatusBarText where
	startRT = pure ()

	exitRT = pure ()

	inputRT _ = lift exitUI >> pure True

-- | Entry point
main :: IO ()
main = launchUI $ ConstructRootWidget $
	pure (StatusBarConfig (Color 0 0 0) (Color 255 255 255),
	      StatusBarText "StatusBarText Bar Text")
