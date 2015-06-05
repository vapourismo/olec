{-# LANGUAGE ExistentialQuantification #-}

module Olec.Interface.Widgets (
	-- * Slot
	Slot,
	newSlot,
	updateSlot,
	useSlot,

	-- * Widget Class
	Widget (..),

	-- * Utilities
	SimpleWidget,
	newSimpleWidget
) where

import Data.IORef

import Olec.Interface.Types
import Olec.Interface.Image

-- | Layout Slot
data Slot = forall o. (Output o) => Slot o (IORef (Position, Size))

-- | Create a new "Slot". It is initially invisible.
newSlot :: (Output o) => o -> IO Slot
newSlot out =
	Slot out <$> newIORef ((0, 0), (0, 0))

-- | Change the bounds associated with the "Slot".
updateSlot :: Slot -> Position -> Size -> IO ()
updateSlot (Slot _ ref) origin size =
	writeIORef ref (origin, size)

-- | Paint something in the designated area associated with the "Slot".
useSlot :: (Visual a) => Slot -> a -> IO ()
useSlot (Slot out ref) w = do
	(origin, size) <- readIORef ref
	when (size /= (0, 0)) $
		outputImage out origin =<< runVisualiser size (visualize w)

-- | A widget which can be displayed on the screen.
class Widget a where
	-- | Update the widget's rectangle.
	update :: a -> Position -> Size -> IO ()

	-- | Render the widget to the screen.
	paint :: a -> IO ()

-- | Wraps a type which does not require an internal layout.
data SimpleWidget a = SimpleWidget a Slot

instance (Visual a) => Widget (SimpleWidget a) where
	update (SimpleWidget _ slot) =
		updateSlot slot

	paint (SimpleWidget w slot) =
		useSlot slot w

-- | Wrap a type to satisfy the "Widget" type class.
--   Ideally this type shall implement "Visual".
newSimpleWidget :: (Output o) => o -> a -> IO (SimpleWidget a)
newSimpleWidget out w =
	SimpleWidget w <$> newSlot out
