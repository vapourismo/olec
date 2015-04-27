{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Vty.Attributes
import Olec.Runtime

-- |
renderInfo :: Attr -> Renderer a
renderInfo attr = do
	(w, h) <- getCanvasSize
	drawString attr (show w ++ " x " ++ show h)

-- |
defaultRenderer :: Renderer a
defaultRenderer =
	alignVertically [
		LeftOver (renderInfo (mkAttr 1 7)),
		Absolute 1 (renderInfo (mkAttr 7 1))
	]

-- |
defaultRuntime :: Runtime () () ()
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
main = evalRuntime_ defaultRuntime defaultRenderer ()
