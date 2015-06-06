{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Concurrent

import Olec.Event.Keys
import Olec.Interface.GTK

-- |
data MainUI = MainUI (KeyMapRef ())

-- |
keyHandler :: MainUI -> KeySource -> IO Bool
keyHandler (MainUI ref) =
	handleKeyEventWithRef ref ()

-- | Entry Point
main :: IO ()
main = do
	iface <- newInterface

	src <- newChan
	registerKeyHandler iface (writeChan src)

	let km = bindKeys $
		bind "Control-q" (const exitInterface)

	forkIO (forever (handleKeyEvent km () src >>= print))

	runInterface
