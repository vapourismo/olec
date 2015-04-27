module Olec.Interface (
	makeInterface
) where

import Control.Exception
import Control.Concurrent

--import qualified Data.Text as T

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import System.Posix.Types
import System.Posix.Terminal

import Olec.Events
import Olec.Terminal

-- | Create the main user interface
makeInterface :: IO (Chan (Event e), Fd)
makeInterface = do
	initGUI
	eventChan <- newChan

	-- Remove style classes
	mbScreen <- screenGetDefault
	flip (maybe (return ())) mbScreen $ \ screen -> do
		cssProvider <- cssProviderNew
		cssProviderLoadFromString cssProvider "VteTerminal { padding: 6px; }"
		styleContextAddProviderForScreen screen cssProvider 800

	-- Main window
	win <- windowNew
	set win [windowTitle := "Olec Text Editor"]

	-- Box
	box <- vBoxNew False 0
	containerAdd win box

	-- Terminal
	(Fd ptm, pts) <- openPseudoTerminal
	term <- newTerminal ptm
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

	return (eventChan, pts)
