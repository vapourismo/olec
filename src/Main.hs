module Main (main) where

import Control.Exception
import Control.Concurrent

import Graphics.UI.Gtk

import System.Posix.Types
import System.Posix.Terminal

import Olec.Events
import Olec.Render
import Olec.Terminal
import Olec.Application

--newtype EventLogWidget = EventLogWidget [Event]

--instance Visual EventLogWidget where
--	render (EventLogWidget events) (_, height) =
--		vertCat (map (string mempty . show) (reverse (take height events)))

--instance Olec.Application.Widget EventLogWidget where
--	onKeyPress (EventLogWidget evs) m k =
--		return (EventLogWidget (KeyPress m k : evs))

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
	forkOS (finally mainGUI (writeChan eventChan ExitRequest))

	-- Launch application
	terminalApplication pts eventChan $
		VLayout [LeftOver (Raster '1'), Relative 0.5 (Raster '2')]
