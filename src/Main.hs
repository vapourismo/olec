{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Concurrent

import qualified Graphics.Vty as Vty

import Olec.Runtime

-- | Entry point
main :: IO ()
main =
	run (forkRuntime parRuntime >> defaultRuntime) defaultRenderer []
	where
		parRuntime =
			forever $ do
				liftIO (threadDelay 1000000)
				modify ("Tick" :)
				render

		defaultRuntime = do
			e <- fetchEvent
			case e of
				KeyPress m k
					| m == toModifierMask [Control] &&
					  k == toKeyValue "q" ->
						return ()

				ExitRequest ->
					return ()

				_ ->
					modify (show e :) >> render >> defaultRuntime

		defaultRenderer = do
			evs <- getRenderState
			(_, height) <- getCanvasSize
			fmap Vty.vertCat (mapM (drawString mempty) (reverse (take height evs)))
