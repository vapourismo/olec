{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface.Display (
	-- * Display
	Display,
	newDisplay,
	clearDisplay,
	glueWidget
) where

import Control.Exception
import Control.Concurrent.QSem

import Graphics.UI.Gtk hiding (Size, Display, Layout, Widget)

import Olec.Interface.Terminal
import Olec.Interface.Renderer
import Olec.Interface.Widget

-- | Display for a "Terminal".
data Display = Display QSem Terminal

instance Canvas Display where
	canvasSize (Display _ term) = terminalSize term

	canvasOrigin _ = pure (0, 0)

	feedCanvas (Display lock term) buf =
		bracket_ (waitQSem lock) (signalQSem lock) (terminalFeed term buf)

-- | Construct new "Display" which acts as a proxy to a "Terminal".
newDisplay :: Terminal -> IO Display
newDisplay term = do
	lock <- newQSem 1
	pure (Display lock term)

-- | Clear the entire display.
clearDisplay :: Display -> IO ()
clearDisplay (Display lock term) =
	bracket_ (waitQSem lock) (signalQSem lock) (terminalFeed term "\ESC[m\ESC[2J")

-- | Update a "Widget" when "Display" resizes.
glueWidget :: (Widget a) => Display -> a -> IO ()
glueWidget d@(Display _ term) w = do
	on term sizeAllocate (const (update w d))
	update w d
