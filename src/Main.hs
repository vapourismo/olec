{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Concurrent

import qualified Data.Text as T

import Olec.Runtime
import Olec.Components.Layouts

-- |
data StatusBar = StatusBar {
	_sbLeft  :: T.Text,
	_sbRight :: Integer
}

$(makeLenses ''StatusBar)

-- |
data AppState = AppState {
	_asStatusBar :: StatusBar
}

$(makeLenses ''AppState)

-- |
sbRender :: Renderer StatusBar
sbRender = do
	StatusBar left right <- getRenderState
	alignHorizontally
		[
			Absolute 1 space,
			LeftOver $
				splitWeighted
					(drawText (mkAttr 0 5) left)
					space
					(drawString (mkAttr 0 6) (show right)),
			Absolute 1 space
		]
	where
		style = mkAttr 0 7
		space = fillChar style ' '

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
	liftIO (threadDelay 100000)
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
	    (AppState (StatusBar "Left 桃" 2))
