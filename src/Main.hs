{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main (main) where

import Control.Concurrent

import qualified Graphics.Vty as V
import Graphics.UI.Gtk (mainQuit)

import Olec.Events
import Olec.Interface
import Olec.Render

import System.Posix.Types

-- |
makeDisplay :: Fd -> IO V.Vty
makeDisplay pts =
	V.mkVty mempty {
		V.inputFd = Just pts,
		V.outputFd = Just pts
	}

-- |
updateDisplayLoop :: V.Vty -> Chan Event -> Renderer -> IO ()
updateDisplayLoop vty events img = do
	let loop size = do
		event <- readChan events
		case event of
			KeyPress m k | m == toModifierMask [Control] &&
			               k == toKeyValue "q" ->
				mainQuit

			ExitRequest ->
				return ()

			KeyPress _ _ -> do
				V.update vty (renderPicture img size)
				loop size

			Resize width height -> do
				V.update vty (renderPicture img (width, height))
				loop (width, height)

	loop (80, 24)

-- | Entry point
main :: IO ()
main = do
	-- Interface
	(events, pts) <- makeInterface

	-- Display
	display <- makeDisplay pts
	updateDisplayLoop display events $
		alignVertically [
			Relative 0.5 (drawText mempty "A"),
			LeftOver (alignHorizontally [
				LeftOver (drawText mempty "B"),
				LeftOver (drawText mempty "C")
			])
		]
