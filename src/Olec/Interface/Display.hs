{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface.Display (
	-- * Display
	Display,
	newDisplay,
	clearDisplay
) where

import Control.Exception
import Control.Concurrent.QSem

import Olec.Interface.Terminal
import Olec.Interface.Renderer

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
