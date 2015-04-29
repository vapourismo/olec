{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Control.Lens

import qualified Data.Text as T

import Olec.Runtime

-- |
data StatusBar = StatusBar T.Text T.Text T.Text

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
split3Way :: Renderer a -> Renderer a -> Renderer a -> Renderer a
split3Way left center right = do
	centerImage <- center
	alignHorizontally
		[
			LeftOver left,
			Absolute (imageWidth centerImage) (pure centerImage),
			LeftOver (justifyRight right)
		]

-- |
sbRender :: Renderer StatusBar
sbRender = do
	StatusBar left center right <- getRenderState
	alignHorizontally
		[
			Absolute 1 paddingRender,
			LeftOver $
				split3Way
					(drawText style left)
					(drawText style center)
					(drawText style right),
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
asRuntime :: Runtime AppState ()
asRuntime = do
	withRuntime asStatusBar $
		put (StatusBar "Left" "Center" "Right")

	e <- fetchEvent
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
	run asRuntime asRender (AppState (StatusBar T.empty T.empty T.empty))
