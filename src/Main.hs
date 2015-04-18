module Main (main) where

import Control.Concurrent

import Graphics.UI.Gtk

import System.Posix.Types
import System.Posix.Terminal

import Olec.Terminal
import Olec.Events

main :: IO ()
main = do
	initGUI
	eventChan <- newChan

	-- Main window
	win <- windowNew
	set win [windowTitle := "Olec Text Editor"]
	on win objectDestroy mainQuit

	-- Box
	box <- vBoxNew False 0
	containerAdd win box

	-- Terminal
	(Fd ptm, _) <- openPseudoTerminal
	term <- newTerminal ptm
	boxPackStart box term PackGrow 0

	-- Dispatch events
	forwardKeyPressEvents win eventChan
	forwardResizeEvents term eventChan

	let loop = do
		k <- readChan eventChan
		case k of
			KeyPress m 113 | m == toModifierMask [Control] -> mainQuit
			_ -> print k >> loop

	forkIO loop

	-- Run
	widgetShowAll win
	mainGUI
