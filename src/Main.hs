{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Concurrent

import Olec.Event.Keys
import Olec.Interface.GTK

-- | Entry Point
main :: IO ()
main = do
	iface <- newInterface

	-- Keys
	src <- newChan
	registerKeyHandler iface (writeChan src)

	let km = bindKeys $
		bind "Control-q" (const exitInterface)

	forkIO (forever (handleKeyEvent km () src))

	runInterface
