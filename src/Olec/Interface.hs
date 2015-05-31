{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface (
	-- * Initializers
	makeInterface,

	-- * Re-exports
	module ReExport,
) where

import Control.Exception
import Control.Concurrent

import qualified Data.Text as T
import qualified Data.ByteString as B

import Graphics.UI.Gtk hiding (Size)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import System.IO

import System.Posix.IO
import System.Posix.Types
import System.Posix.Terminal

import Olec.Interface.Terminal
import Olec.Interface.Types as ReExport
import Olec.Interface.Events as ReExport
import Olec.Interface.Renderer as ReExport

-- | Launch user interface.
launchUI :: Chan Event -> IO (B.ByteString -> IO (), IO Size)
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
	(Fd ptm, pts) <- openPseudoTerminal
	term <- newTerminal ptm ["#111111"]
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

	pure (terminalFeed term, terminalSize term)

-- | Create the main user interface.
makeInterface :: IO (Chan Event, B.ByteString -> IO (), IO Size)
makeInterface = do
	eventChan <- newChan
	(feedAction, sizeAction) <- launchUI eventChan
	pure (eventChan, feedAction, sizeAction)
