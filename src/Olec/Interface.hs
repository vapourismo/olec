{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface (
	-- * Display
	Display,
	displayLock,
	displayFeed,
	displaySize,
	renderOnDisplay,

	-- * Initializers
	makeInterface,

	-- * Re-exports
	module ReExport,
) where

import Control.Exception
import Control.Concurrent

import qualified Data.Text as T
import qualified Data.ByteString as B

import Graphics.UI.Gtk hiding (Size, Display)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import System.Posix.Types
import System.Posix.Terminal

import Olec.Interface.Terminal
import Olec.Interface.Types as ReExport
import Olec.Interface.Events as ReExport
import Olec.Interface.Renderer as ReExport

-- | Display
data Display = Display {
	displayLock :: QSem,
	displayFeed :: B.ByteString -> IO (),
	displaySize :: IO Size
}

-- | Render something to the screen.
renderOnDisplay :: Display -> Renderer a -> IO a
renderOnDisplay (Display lock feedIO sizeIO) renderer =
	bracket_ (waitQSem lock)
	         (signalQSem lock)
	         (sizeIO >>= runRenderer renderer feedIO (0, 0))


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

	lock <- newQSem 1
	pure (Display lock (terminalFeed term) (terminalSize term))

-- | Create the main user interface.
makeInterface :: IO (Chan Event, Display)
makeInterface = do
	eventChan <- newChan
	display <- launchUI eventChan
	pure (eventChan, display)
