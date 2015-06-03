module Olec.Interface.Events (
	-- * General
	KeyEvent (..),

	-- * Utilities
	Modifier (..),
	toModifierMask,
	toKeyValue,

	-- * Type Class
	EventSource (..),
) where

import Data.Word
import Data.Bits
import Data.List

import qualified Data.Text as T

import Graphics.UI.Gtk hiding (Size, Display, Layout, Widget)

import Olec.Interface.Types

-- | Make a modifier mask from a list of modifiers.
toModifierMask :: [Modifier] -> Word32
toModifierMask = foldl' (.|.) 0 . map (bit . fromEnum)

-- | Fetch key value using it's name.
toKeyValue :: T.Text -> Word32
toKeyValue = keyFromName

-- | An event triggered by a key.
data KeyEvent = KeyPress Word32 Word32
	deriving (Show, Eq, Ord)

-- | Event source
class EventSource a where
	onKeyEvent :: a -> (KeyEvent -> IO Bool) -> IO ()

	onResize :: a -> (Size -> IO ()) -> IO ()
