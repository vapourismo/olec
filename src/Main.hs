{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader

import Data.Metrics

import Olec.Interface.GTK
import Olec.Interface.Types
import Olec.Interface.Image
import Olec.Interface.Events
import Olec.Interface.Widgets

-- | Fill the entire drawing area with the given character.
fillArea :: Style -> Char -> Visualiser
fillArea style c = do
	(w, h) <- ask
	alignVertically (hints w h)
	where
		line w = replicate w c
		hints w h = replicate h (Absolute 1 (drawString style (line w)))

-- | Bind a widget to the screen.
bindWidget :: (EventSource o, Output o, Widget a) => o -> a -> IO ()
bindWidget o w = do
	onResize o $ \ size -> do
		update w (0, 0) size
		paint w

	update w (0, 0) =<< sizeOfOutput o
	paint w

data Test = Test

instance Visual Test where
	visualize _ = fillArea (Style (Color 255 255 255) (Color 255 255 255)) ' '

main :: IO ()
main = do
	o <- newInterface
	onKeyEvent o (\ _ -> True <$ exitInterface)

	h <- newSimpleWidget o Test
	bindWidget o h

	runInterface
