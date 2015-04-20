{-# LANGUAGE OverloadedStrings #-}

module Olec.Application (
	terminalApplication
) where

import Control.Concurrent

import Graphics.UI.Gtk.General.General

import System.Posix.Types

import Olec.Events
import Olec.Render

-- | Render an instance of "Visual".
renderVisual :: (Visual a) => Vty -> a -> Size -> IO ()
renderVisual vty widget size =
	update vty Picture {
		picCursor = NoCursor,
		picLayers = [render' widget size],
		picBackground = ClearBackground
	}

-- | The function name is just a coincidence.
terminalApplication :: (Visual a) => Fd -> Chan Event -> a -> IO ()
terminalApplication pts events widget = do
	vty <- mkVty mempty {
		inputFd = Just pts,
		outputFd = Just pts
	}

	let loop size = do
		event <- readChan events
		case event of
			KeyPress m k | m == toModifierMask [Control] &&
			               k == toKeyValue "q" ->
				mainQuit
			ExitRequest -> return ()
			KeyPress _ _ -> do
				renderVisual vty widget size
				loop size
			Resize width height -> do
				renderVisual vty widget (width, height)
				loop (width, height)

	loop (1, 1)
