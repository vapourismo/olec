module Olec.Visual.Widget (
	-- * General
	Widget (..),

	-- * Flat
	Flat,
	newFlat,
	withFlat,
	paintFlat,
) where

import Data.Handle

import Olec.Visual
import Olec.Visual.Slot
import Olec.Visual.Layout

class (LayoutElement a) => Widget a where
	paint :: a -> IO ()

-- | Widget without any sub-widgets.
data Flat s = Flat Slot s

instance LayoutElement (Flat s) where
	layout (Flat slot _) = layout slot

instance (Paintable s) => Widget (Flat s) where
	paint (Flat slot st) =
		paintSlot slot (toPainter st)

-- | Make a new "Flat" handle.
newFlat :: (Handle h, Canvas o) => o -> s -> IO (Flat (h s))
newFlat out s =
	Flat <$> toSlot out <*> newHandle s

-- | Do something with the inner state.
withFlat :: Flat s -> (s -> IO b) -> IO b
withFlat (Flat _ s) action = action s

-- | Paint the "Flat" widget.
paintFlat :: (Paintable s) => Flat s -> IO ()
paintFlat (Flat slot s) = paintSlot slot (toPainter s)
