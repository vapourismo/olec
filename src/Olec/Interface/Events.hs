module Olec.Interface.Events (
	KeyEvent (..),
	Modifier (..),
	isModifier,
	toModifierMask,
	toKeyValue,
) where

import Data.Word
import Data.Bits
import Data.List

import qualified Data.Text as T

import Graphics.UI.Gtk hiding (Size, Display, Layout, Widget)

-- | Check if the given key value is single modifier key stroke.
isModifier :: Word32 -> Bool
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
