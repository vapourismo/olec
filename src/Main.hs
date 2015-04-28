{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import Graphics.Vty.Attributes

import Olec.Runtime

-- |
data StatusBar = StatusBar {
	sbStatusText :: T.Text
}

-- |
data AppState = AppState {
	asStatusBar :: StatusBar
}

-- |
renderStatusBar :: Renderer StatusBar
renderStatusBar =
	alignHorizontally [
		Absolute 1 spacer,
		LeftOver (getRenderState >>= drawText style . sbStatusText),
		Absolute 1 spacer
	]
	where
		style = mkAttr 0 7
		spacer = fillChar style ' '

-- |
renderInfo :: Attr -> Renderer a
renderInfo attr = do
	(w, h) <- getCanvasSize
	drawString attr (show w ++ " x " ++ show h)

-- |
renderAppState :: Renderer AppState
renderAppState =
	alignVertically [
		LeftOver (renderInfo (mkAttr 7 1)),
		Absolute 1 (withRenderer asStatusBar renderStatusBar)
	]

-- |
defaultRuntime :: Runtime AppState () ()
defaultRuntime =
	render >> eventLoop
	where
		eventLoop = do
			e <- fetchEvent
			case e of
				RExitRequest ->
					return ()

				RKeyPress m k | k == toKeyValue "q" && m == toModifierMask [Control] ->
					requestExit

				_ -> liftIO (print e) >> eventLoop

-- | Entry point
main :: IO ()
main =
	evalRuntime_ defaultRuntime renderAppState (
		AppState {
			asStatusBar = StatusBar {
				sbStatusText = "Nothing yet, oasdohaiopmj23cdnh0c79dshsdh89o0sdgfhuioasdfhuiopasdfhjiohjklsdfhjklsdfhjksdfhjksdfhjksdfhjksdfhjksdfhjkdsfoasdohaiopmj23cdnh0c79dshsdh89o0sdgfhuioasdfhuiopasdfhjiohjklsdfhjklsdfhjksdfhjksdfhjksdfhjksdfhjksdfhjkdsfoasdohaiopmj23cdnh0c79dshsdh89o0sdgfhuioasdfhuiopasdfhjiohjklsdfhjklsdfhjksdfhjksdfhjksdfhjksdfhjksdfhjkdsf"
			}
		}
	)
