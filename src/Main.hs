{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Concurrent

import qualified Data.Text as T

import Olec.Runtime

-- |
data StatusBar = StatusBar {
	_sbLeft   :: T.Text,
	_sbRight :: Integer
}

$(makeLenses ''StatusBar)

-- |
data AppState = AppState {
	_asStatusBar :: StatusBar
}

$(makeLenses ''AppState)

-- |
justifyRight :: Renderer a -> Renderer a
justifyRight renderer = do
	img <- renderer
	alignHorizontally [LeftOver emptyRenderer, Absolute (imageWidth img) (pure img)]

-- |
split2Way :: Renderer a -> Renderer a -> Renderer a
split2Way left right =
	alignHorizontally [LeftOver left, LeftOver (justifyRight right)]

-- |
sbRender :: Renderer StatusBar
sbRender = do
	StatusBar left right <- getRenderState
	alignHorizontally
		[
			Absolute 1 paddingRender,
			LeftOver $
				split2Way
					(drawText style left)
					(drawString style (show right)),
			Absolute 1 paddingRender
		]
	where
		style = mkAttr 0 7
		paddingRender = fillChar style ' '

-- |
asRender :: Renderer AppState
asRender =
	alignVertically
		[
			LeftOver emptyRenderer,
			Absolute 1 (withRenderer asStatusBar sbRender)
		]

-- |
sbRuntime :: Runtime StatusBar ()
sbRuntime = forever $ do
	liftIO (threadDelay 1)
	sbRight %= (* 2)
	render

-- |
asRuntime :: Runtime AppState ()
asRuntime = do
	e <- ask
	case e of
		KeyPress m k
			| m == toModifierMask [Control] &&
			  k == toKeyValue "q" ->
				return ()

		ExitRequest ->
			return ()

		Resize _ _ ->
			render >> asRuntime

		KeyPress _ _ ->
			render >> asRuntime

-- | Entry point
main :: IO ()
main =
	run (forkRuntime (withRuntime asStatusBar sbRuntime) >> asRuntime)
	    asRender
	    (AppState (StatusBar "Left" 2))
