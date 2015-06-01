module Olec.Interface.Events (
	Event (..),

	forwardKeyPressEvents,

	toModifierMask,
	toKeyValue,

	Modifier (..)
) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import Data.Bits
import Data.List
import Data.Word

import qualified Data.Text as T

import Graphics.UI.Gtk

-- | Application event
data Event
	= KeyPress Word32 Word32
	| ExitRequest
	deriving (Show, Eq, Ord)

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

-- | Forward key events to an event channel.
forwardKeyPressEvents :: (WidgetClass w) => w -> Chan Event -> IO ()
forwardKeyPressEvents widget chan =
	void $ on widget keyPressEvent $ True <$ do
		eval <- eventKeyVal
		emod <- eventModifier

		unless (isModifier eval) . lift $
			writeChan chan (KeyPress (toModifierMask emod) eval)
