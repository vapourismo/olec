module Main (main) where

import Control.Concurrent

import Graphics.UI.Gtk

import System.Posix.Types
import System.Posix.Terminal

import Olec.Events
import Olec.Terminal
import Olec.Application

main :: IO ()
main = do
	initGUI
	eventChan <- newChan

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
	forkOS mainGUI

	-- Launch application
	terminalApplication pts eventChan
	mainQuit
