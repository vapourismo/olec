{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Control.Lens

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
	run asRuntime asRender
	    (AppState (StatusBar "Left 桃" "Right 桃" (mkAttr 0 7)))
