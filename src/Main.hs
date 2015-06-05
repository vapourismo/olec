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

	src <- newChan
	registerKeyHandler iface (writeChan src)

	let km = bindKeys $ do
		bind "Control-q" (const exitInterface)
		bind "Control-Up" (const (putStrLn "Hello World"))

	forkIO (forever (handleKeyEvent km () src >>= print))

	runInterface
