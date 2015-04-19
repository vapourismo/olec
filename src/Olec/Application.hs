{-# LANGUAGE OverloadedStrings #-}

module Olec.Application (
	KeyEventRecipient (..),

	terminalApplication,

	module Olec.Events,
	module Olec.Render
) where

import Control.Concurrent

import Data.Word

import Graphics.UI.Gtk.General.General

import System.Posix.Types

import Olec.Events
import Olec.Render

class KeyEventRecipient a where
	onKeyPress :: a -> Word32 -> Word32 -> IO a

-- | Render an instance of "Visual".
renderVisual :: (Visual a) => Vty -> a -> Size -> IO ()
renderVisual vty widget size =
	update vty Picture {
		picCursor = NoCursor,
		picLayers = [render' widget size],
		picBackground = ClearBackground
	}

-- | The function name is just a coincidence.
terminalApplication :: (KeyEventRecipient a, Visual a) => Fd -> Chan Event -> a -> IO ()
terminalApplication pts events initWidget = do
	vty <- mkVty mempty {
		inputFd = Just pts,
		outputFd = Just pts
	}

	let loop widget size = do
		event <- readChan events
		case event of
			KeyPress m k | m == toModifierMask [Control] &&
			               k == toKeyValue "q" ->
				mainQuit
			ExitRequest -> return ()
			KeyPress m k -> do
				widget' <- onKeyPress widget m k
				renderVisual vty widget' size
				loop widget' size
			Resize width height -> do
				renderVisual vty widget (width, height)
				loop widget (width, height)

	loop initWidget (1, 1)
