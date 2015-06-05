module Olec.Event.Keys (
	-- * General
	KeyEvent (..),

	-- * Utilities
	Modifier (..),
	toModifierMask,
	toKeyValue,

	-- * Key map
	KeyMap,
	KeySource,
	handleKeyEvent,

	KeyBinder,
	bindKeys,
	bind,
	bind',
	nest
) where

import Control.Concurrent.Chan
import Control.Monad.State.Strict

import Data.Word
import Data.Bits
import Data.List
import Data.Char hiding (Control)
import Data.String

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Graphics.UI.Gtk hiding (Size, Display, Layout, Widget)

import System.Timeout

import Olec.Config

-- | Make a modifier mask from a list of modifiers.
toModifierMask :: [Modifier] -> Word32
toModifierMask =
	foldl' (.|.) 0 . map (bit . fromEnum)

-- | Fetch key value using it's name.
toKeyValue :: T.Text -> Word32
toKeyValue = keyFromName

-- | An event triggered by a key.
data KeyEvent = KeyEvent Word32 Word32
	deriving (Show, Eq, Ord)

instance IsString KeyEvent where
	fromString str =
		case split str of
			[]  -> KeyEvent 0 0
			[x] -> KeyEvent 0 (toKeyValue (T.pack x))
			mix -> KeyEvent (toModifierMask (map (toModifier . map toLower) (init mix)))
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

-- | Key bindings
data KeyMap a
	= KeyHandler (a -> KeySource -> IO Bool)
	| KeyMap (M.Map KeyEvent (KeyMap a))

-- | Key event source
type KeySource = Chan KeyEvent

-- | Handle a key event using the given "KeyMap".
--   Returns "False" if no handler could be found.
handleKeyEvent :: KeyMap a -> a -> KeySource -> IO Bool
handleKeyEvent (KeyHandler handle) x src =
	handle x src
handleKeyEvent (KeyMap km) x src = do
	ev <- readChan src
	maybe (pure False) handleKeyEvent2 (M.lookup ev km)
	where
		handleKeyEvent2 (KeyHandler handle) =
			handle x src
		handleKeyEvent2 (KeyMap km2) = do
			mbEvent <- timeout configKeyTimeout (readChan src)
			maybe (pure False) handleKeyEvent2 (mbEvent >>= flip M.lookup km2)

-- | Key binder utility.
type KeyBinder a = State (M.Map KeyEvent (KeyMap a)) ()

-- | Run the "KeyBinder" to generate a "KeyMap".
bindKeys :: KeyBinder a -> KeyMap a
bindKeys b = KeyMap (execState b M.empty)

-- | Bind an action to a "KeyEvent".
bind :: KeyEvent -> (a -> IO ()) -> KeyBinder a
bind ev handle =
	bind' ev (\ x _ -> True <$ handle x)

-- | Special version of "bind".
bind' :: KeyEvent -> (a -> KeySource -> IO Bool) -> KeyBinder a
bind' ev handle =
	modify (M.insert ev (KeyHandler handle))

-- | Create a nested "KeyMap".
nest :: KeyEvent -> KeyBinder a -> KeyBinder a
nest ev (StateT f) =
	StateT $ \ m ->
		fmap (\ nm -> M.insertWith merge ev (KeyMap nm) m) <$> f M.empty
	where
		merge (KeyMap a) (KeyMap b) = KeyMap (M.unionWith merge a b)
		merge _ x = x
