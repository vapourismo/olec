module Olec.Interface.Widget (
	-- * Widgets
	Widget (..),
	update,

	-- * Flat Widget
	FlatWidget,
	newFlatWidget
) where

import Olec.Interface.Display
import Olec.Interface.Layout
import Olec.Interface.Renderer

-- | Types which might appear on screen
class Widget a where
	layout :: a -> Layout ()
	paint :: a -> IO ()

-- | Update a "Widget" to fit inside the given "Canvas".
--   This will repaint the "Widget" aswell.
update :: (Widget a, Canvas c) => a -> c -> IO ()
update w c = do
	runLayout (layout w) c
	paint w

-- | A widget which does not have sub-widgets.
data FlatWidget a = FlatWidget a LayoutDelegate

instance (Visual a) => Visual (FlatWidget a) where
	visualize (FlatWidget w _) =
		visualize w

instance (Visual a) => Widget (FlatWidget a) where
	layout (FlatWidget _ del) =
		delegateLayout del

	paint (FlatWidget w del) =
		runRenderer (visualize w) del

-- | Wrap a value into the "FlatWidget" type.
newFlatWidget :: Display -> a -> IO (FlatWidget a)
newFlatWidget d w =
	FlatWidget w <$> toLayoutDelegate d
