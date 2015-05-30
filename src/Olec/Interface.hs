module Olec.Interface (
	makeInterface,
	makeRawInterface,
	module Olec.Interface.Events
) where

import Control.Monad
import Control.Exception
import Control.Concurrent

import qualified Graphics.Vty as V

import Graphics.UI.Gtk hiding (Size)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import System.Posix.Types
import System.Posix.Terminal

import Olec.Interface.Events
import Olec.Interface.Terminal

import Olec.Render

-- | Launch user interface.
launcUI :: Chan Event -> IO (Fd, IO Size)
launcUI eventChan = do
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

	pure (pts, terminalSize term)

-- | Create the main user interface.
makeInterface :: (Visual v) => IO v -> IO (Chan Event, IO ())
makeInterface stateReader = do
	eventChan <- newChan
	(pts, sizeAction) <- launcUI eventChan

	-- Setup Vty
	displayMVar <- newMVar =<< V.mkVty mempty {
		V.inputFd = Just pts,
		V.outputFd = Just pts
	}

	requestsMVar <- newMVar 0 :: IO (MVar Word)

	-- An IO action which updates the display
	let updateAction = do
		modifyMVar_ requestsMVar (\ x -> pure $! x + 1)
		withMVar displayMVar $ \ display -> do
			counter <- modifyMVar requestsMVar (\ x -> pure (0, x))
			when (counter > 0) $
				RenderContext <$> sizeAction
				              <*> stateReader
				              >>= V.update display . renderPicture mkRenderer

	return (eventChan, updateAction)

-- | Create the main user interface.
makeRawInterface :: IO (Chan Event, Fd, IO Size)
makeRawInterface = do
	eventChan <- newChan
	(pts, sizeAction) <- launcUI eventChan
	pure (eventChan, pts, sizeAction)
