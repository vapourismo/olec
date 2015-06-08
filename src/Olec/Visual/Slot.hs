{-# LANGUAGE ExistentialQuantification #-}

module Olec.Visual.Slot (
	-- * Slot
	Slot,
	toSlot,
	updateSlot,
	paintSlot,
) where

import Control.Monad

import Data.IORef

import Olec.Visual
import Olec.Visual.Layout

-- | Layout Slot
data Slot = forall o. (Canvas o) => Slot o (IORef (Position, Size))

instance Layout Slot where
	updateLayout = updateSlot

-- | Create a new "Slot". The slot initially occupies the entire canvas.
toSlot :: (Canvas o) => o -> IO Slot
toSlot out = do
	size <- sizeOfCanvas out
	Slot out <$> newIORef ((0, 0), size)

-- | Update the "Slot" with new bounds.
updateSlot :: Slot -> Position -> Size -> IO ()
updateSlot (Slot _ ref) origin size =
	writeIORef ref (origin, size)

-- | Paint something in the designated area associated with the "Slot".
paintSlot :: Slot -> Painter -> IO ()
paintSlot (Slot out ref) vis = do
	(origin, size) <- readIORef ref
	when (size /= (0, 0)) (renderImage out origin =<< paintImage vis size)
