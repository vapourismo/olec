{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad

import Data.Default

import Olec.Runtime
import Olec.Components.StatusBar

-- |
data AppState = AppState {
	_asStatusBar :: StatusBar
}

makeLenses ''AppState

-- |
asRender :: Renderer AppState
asRender =
	alignVertically
		[
			LeftOver emptyRenderer,
			Absolute 1 (withRenderer asStatusBar renderStatusBar)
		]

-- |
asRuntime :: RemoteRuntime -> Runtime AppState ()
asRuntime mf = do
	e <- ask
	case e of
		KeyPress m k
			| m == toModifierMask [Control] &&
			  k == toKeyValue "q" ->
				return ()

		ExitRequest ->
			return ()

		Resize _ _ ->
			asRuntime mf

		_ -> do
			forwardEvent e mf
			asRuntime mf

-- | Entry point
main :: IO ()
main =
	run runtime asRender
	    (AppState def)
	where
		runtime = do
			forkRuntime (withRuntime asStatusBar runStatusBar)
			mf <- forkRuntime (forever (ask >>= liftIO . print))
			asRuntime mf

