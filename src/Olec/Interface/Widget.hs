{-# LANGUAGE TypeFamilies #-}

module Olec.Interface.Widget (
	-- * Widgets
	Widget (..),
	RootWidget (..),
	update,

	-- * Flat Widget
	FlatWidget,
	newFlatWidget
) where

import Olec.Interface.Layout
import Olec.Interface.Renderer
import Olec.Interface.Events

-- | Types which might appear on screen
class Widget a where
	layout :: a -> Layout ()
	paint :: a -> IO ()

-- | This will be glued to the screen.
class (Widget a) => RootWidget a where
	data Setup a

	setup :: (Canvas c) => c -> Setup a -> IO a

	input :: a -> KeyEvent -> IO Bool

	exit :: a -> IO ()

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
newFlatWidget :: (Canvas c) => c -> a -> IO (FlatWidget a)
newFlatWidget d w =
	FlatWidget w <$> toLayoutDelegate d
