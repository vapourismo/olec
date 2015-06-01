module Olec.Interface (
	-- * Interface
	makeInterface,

	-- * Re-exports
	module ReExport,
) where

import Control.Monad
import Control.Exception

import Control.Concurrent

import Graphics.UI.Gtk hiding (Size, Display, Layout)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import Olec.Interface.Terminal
import Olec.Interface.Types as ReExport
import Olec.Interface.Events as ReExport
import Olec.Interface.Renderer as ReExport
import Olec.Interface.Layout as ReExport
import Olec.Interface.Display as ReExport
import Olec.Interface.Widget as ReExport

-- | Launch user interface.
launchUI :: Chan Event -> MVar () -> IO Display
launchUI eventChan resizeVar = do
	initGUI

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
	term <- newTerminal
	boxPackStart box term PackGrow 0

	-- Dispatch events
	forwardKeyPressEvents win eventChan

	on term sizeAllocate (\ _ -> void (tryPutMVar resizeVar ()))

	on win objectDestroy mainQuit
	on term buttonPressEvent (return True)
	on term buttonReleaseEvent (return True)

	-- Show interface
	widgetShowAll win
	forkOS (finally mainGUI (writeChan eventChan ExitRequest))

	newDisplay term

-- | Create the main user interface.
makeInterface :: IO (Chan Event, MVar (), Display)
makeInterface = do
	eventChan <- newChan
	resizeVar <- newEmptyMVar
	display <- launchUI eventChan resizeVar
	pure (eventChan, resizeVar, display)
