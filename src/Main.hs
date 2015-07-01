{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader

import Control.Concurrent

import Olec.Visual.Types
import Olec.Visual.Image

import Olec.Interface.Pango

import System.Random

randomColor :: IO Color
randomColor = Color <$> randomIO <*> randomIO <*> randomIO

randomChar :: Painter
randomChar = do
	style <- liftIO (Style <$> randomColor <*> randomColor)
	text style "X"

main :: IO ()
main = do
	ui <- newInterface

	setFont ui "Inconsolata 10.5"
	setPainter ui "#1a1a1a" $ do
		(w, h) <- ask
		vcat (replicate h (hcat (replicate w randomChar)))

	forkIO $ forever $ do
		threadDelay 1000000
		requestDraw ui

	runInterface
