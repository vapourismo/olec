module Olec.Event.Keys (
	-- * General
	KeyEvent (..),

	-- * Utilities
	Modifier (..),
	toModifierMask,
	toKeyValue,
) where

import Data.Word
import Data.Bits
import Data.List
import Data.Char hiding (Control)
import Data.String

import qualified Data.Text as T

import Graphics.UI.Gtk hiding (Size, Display, Layout, Widget)

-- | Make a modifier mask from a list of modifiers.
toModifierMask :: [Modifier] -> Word32
toModifierMask =
	foldl' (.|.) 0 . map (bit . fromEnum)

-- | Fetch key value using it's name.
toKeyValue :: T.Text -> Word32
toKeyValue = keyFromName

-- | An event triggered by a key.
data KeyEvent = KeyPress Word32 Word32
	deriving (Show, Eq, Ord)

instance IsString KeyEvent where
	fromString str =
		case split str of
			[]  -> KeyPress 0 0
			[x] -> KeyPress 0 (toKeyValue (T.pack x))
			mix -> KeyPress (toModifierMask (map (toModifier . map toLower) (init mix)))
			                (toKeyValue (T.pack (last mix)))
		where
			split xs =
				case break (== '-') xs of
					(x, []) -> [x]
					(x, _ : ys) -> x : split ys

			toModifier "control" = Control
			toModifier "shift"   = Shift
			toModifier "meta"    = Meta
			toModifier "alt"     = Alt
			toModifier "alt1"    = Alt
			toModifier "alt2"    = Alt2
			toModifier "alt3"    = Alt3
			toModifier "alt4"    = Alt4
			toModifier "alt5"    = Alt5
			toModifier "super"   = Super
			toModifier "hyper"   = Hyper
			toModifier x         = error ("Unknown modifier '" ++ x ++ "'")
