module Olec.Interface.Events (
	Event (..),

	forwardKeyPressEvents,
	forwardResizeEvents,

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
import Data.IORef
import qualified Data.Text as T

import Graphics.UI.Gtk

import Olec.Interface.Terminal

-- | Application event
data Event
	= Resize Int Int
	| KeyPress Word32 Word32
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

-- | Forward resize events to an event channel.
forwardResizeEvents :: Terminal -> Chan Event -> IO ()
forwardResizeEvents term chan = do
	dimRef <- newIORef (0, 0)
	void $ on term sizeAllocate $ \ _ -> do
		tup <- terminalSize term
		tup' <- readIORef dimRef
		when (tup /= tup') $ do
			writeIORef dimRef tup
			writeChan chan (Resize (fromIntegral (fst tup)) (fromIntegral (snd tup)))
