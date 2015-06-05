{-# LANGUAGE ExistentialQuantification #-}

module Olec.Interface.Slot (
	-- * Slot
	Slot,
	newSlot,
	updateSlot,
	paintSlot,
) where

import Control.Monad

import Data.IORef

import Olec.Interface.Types
import Olec.Interface.Image

-- | Layout Slot
data Slot = forall o. (Canvas o) => Slot o (IORef (Position, Size))

-- | Create a new "Slot". It is initially invisible.
newSlot :: (Canvas o) => o -> IO Slot
newSlot out =
	Slot out <$> newIORef ((0, 0), (0, 0))

-- | Change the bounds associated with the "Slot".
updateSlot :: Slot -> Position -> Size -> IO ()
updateSlot (Slot _ ref) origin size =
	writeIORef ref (origin, size)

-- | Paint something in the designated area associated with the "Slot".
paintSlot :: Slot -> Painter -> IO ()
paintSlot (Slot out ref) vis = do
	(origin, size) <- readIORef ref
	when (size /= (0, 0)) (renderImage out origin =<< paintImage vis size)
