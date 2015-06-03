{-# LANGUAGE TypeFamilies #-}

module Olec.Interface (
	-- * Interface
	RootWidget (..),
	launchUI,
	exitUI,

	-- * Re-exports
	module ReExport,
) where

import Control.Exception
import Control.Monad.Trans

import Graphics.UI.Gtk hiding (Size, Display, Layout, Widget)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import Olec.Interface.Terminal
import Olec.Interface.Types as ReExport
import Olec.Interface.Renderer as ReExport
import Olec.Interface.Layout as ReExport
import Olec.Interface.Display as ReExport
import Olec.Interface.Widget as ReExport
import Olec.Interface.Component as ReExport
import Olec.Interface.Events as ReExport

-- | Launch user interface.
launchUI :: (RootWidget a) => Setup a -> IO ()
launchUI s = do
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

	-- Display
	display <- newDisplay term
	widget <- setup display s

	-- Key events
	on term keyPressEvent $ do
		eval <- eventKeyVal
		emod <- eventModifier

		if isModifier eval then
			pure False
		else
			lift (input widget (KeyPress (toModifierMask emod) eval))

	-- Resize events
	on term sizeAllocate (const (clearDisplay display >> update widget display))

	-- Other events
	on win objectDestroy mainQuit
	on term buttonPressEvent (return True)
	on term buttonReleaseEvent (return True)

	-- Show interface
	widgetShowAll win
	finally mainGUI (exit widget)

-- | Exit the UI main loop.
exitUI :: IO ()
exitUI = mainQuit
