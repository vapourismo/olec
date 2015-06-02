{-# LANGUAGE TypeFamilies #-}

module Olec.Interface (
	-- * Interface
	RootWidget (..),
	launchUI,
	exitUI,

	-- * Events
	KeyEvent (..),
	Modifier (..),
	toModifierMask,
	toKeyValue,

	-- * Re-exports
	module ReExport,
) where

import Control.Exception
import Control.Monad.Trans

import Data.Bits
import Data.Word
import Data.List

import qualified Data.Text as T

import Graphics.UI.Gtk hiding (Size, Display, Layout, Widget)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import Olec.Interface.Terminal
import Olec.Interface.Types as ReExport
import Olec.Interface.Renderer as ReExport
import Olec.Interface.Layout as ReExport
import Olec.Interface.Display as ReExport
import Olec.Interface.Widget as ReExport

-- | Check if the given key value is single modifier key stroke.
isModifier :: KeyVal -> Bool
isModifier key =
	(0xffe1 <= key && key <= 0xffee) ||
	(0xfe01 <= key && key <= 0xfe0f) ||
	(0xfe11 <= key && key <= 0xfe13) ||
	key == 0xff7e

-- | Make a modifier mask from a list of modifiers.
toModifierMask :: [Modifier] -> Word32
toModifierMask = foldl' (.|.) 0 . map (bit . fromEnum)

-- | Fetch key value using it's name.
toKeyValue :: T.Text -> Word32
toKeyValue = keyFromName

-- | An event triggered by a key.
data KeyEvent = KeyPress Word32 Word32
	deriving (Show, Eq, Ord)

-- | This will be glued to the screen.
class (Widget a) => RootWidget a where
	data Setup a

	setup :: Display -> Setup a -> IO a

	input :: a -> KeyEvent -> IO Bool

	exit :: a -> IO ()

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
