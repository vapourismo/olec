{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface (
	-- * Interface
	makeInterface,

	-- * Re-exports
	module ReExport,
) where

import Control.Exception
import Control.Concurrent

import qualified Data.Text as T

import Graphics.UI.Gtk hiding (Size, Display, Layout)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import Olec.Interface.Terminal
import Olec.Interface.Types as ReExport
import Olec.Interface.Events as ReExport
import Olec.Interface.Renderer as ReExport
import Olec.Interface.Layout as ReExport
import Olec.Interface.Display as ReExport

-- | Launch user interface.
launchUI :: Chan Event -> IO Display
launchUI eventChan = do
	initGUI

	-- Remove style classes
	mbScreen <- screenGetDefault
	flip (maybe (return ())) mbScreen $ \ screen -> do
		cssProvider <- cssProviderNew
		cssProviderLoadFromString cssProvider ("VteTerminal { padding: 6px; }" :: T.Text)
		styleContextAddProviderForScreen screen cssProvider 800

	-- Main window
	win <- windowNew
	set win [windowTitle := ("Olec Text Editor" :: T.Text)]

	-- Box
	box <- vBoxNew False 0
	containerAdd win box

	-- Terminal
	term <- newTerminal
	boxPackStart box term PackGrow 0

	-- Dispatch events
	forwardKeyPressEvents win eventChan
	forwardResizeEvents term eventChan

	on win objectDestroy mainQuit
	on term buttonPressEvent (return True)
	on term buttonReleaseEvent (return True)

	-- Show interface
	widgetShowAll win
	forkOS (finally mainGUI (writeChan eventChan ExitRequest))

	newDisplay term

-- | Create the main user interface.
makeInterface :: IO (Chan Event, Display)
makeInterface = do
	eventChan <- newChan
	display <- launchUI eventChan
	pure (eventChan, display)
