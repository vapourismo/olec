{-# LANGUAGE ExistentialQuantification #-}

module Olec.Visual.Slot (
	-- * Slot
	Slot,
	toSlot,
	paintSlot,
) where

import Control.Monad
import Control.Monad.Reader

import Data.IORef

import Olec.Visual
import Olec.Visual.Layout

-- | Layout Slot
data Slot = forall o. (Canvas o) => Slot o (IORef (Position, Size))

instance LayoutElement Slot where
	layout (Slot _ ref) =
		ask >>= liftIO . writeIORef ref

-- | Create a new "Slot". The slot initially occupies the entire canvas.
toSlot :: (Canvas o) => o -> IO Slot
toSlot out = do
	size <- sizeOfCanvas out
	Slot out <$> newIORef ((0, 0), size)

-- | Paint something in the designated area associated with the "Slot".
paintSlot :: Slot -> Painter -> IO ()
paintSlot (Slot out ref) vis = do
	(origin, size) <- readIORef ref
	when (size /= (0, 0)) (renderImage out origin =<< paintImage vis size)
