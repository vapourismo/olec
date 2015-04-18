module Olec.Events (
	Event (..),
	forwardKeyPressEvents,
	forwardResizeEvents
) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import Data.IORef

import Graphics.UI.Gtk

import Olec.Terminal

-- | Application event
data Event
	= Resize Int Int
	| KeyPress [Modifier] KeyVal
	deriving (Show)

-- | Check if the given key value is single modifier key stroke.
isModifier :: KeyVal -> Bool
isModifier key =
	(0xffe1 <= key && key <= 0xffee) ||
	(0xfe01 <= key && key <= 0xfe0f) ||
	(0xfe11 <= key && key <= 0xfe13) ||
	key == 0xff7e

-- | Forward key events to an event channel.
forwardKeyPressEvents :: (WidgetClass widget) => widget -> Chan Event -> IO ()
forwardKeyPressEvents widget chan =
	void $ on widget keyPressEvent $ True <$ do
		eval <- eventKeyVal
		emod <- eventModifier

		unless (isModifier eval) . lift $ do
			writeChan chan (KeyPress emod eval)

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
