{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Reader

import Control.Concurrent

import Olec.Event.Keys
import Olec.Visual
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

	-- Visual
	let style = Style "#ffffff" "#ff0000"

	let firstLayer = do
		(w, h) <- ask
		vcat (replicate h (string style (replicate w 'x')))

	let secondLayer = center (text (Style "#000000" "#ffffff") "Hello World")

	registerResizeHandler iface $
		paintImage (layered [firstLayer, secondLayer]) >=> renderImage iface (0, 0)

	runInterface
