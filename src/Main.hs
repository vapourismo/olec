{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader

import Data.IORef
import Data.Metrics

import qualified Data.Text as T

import Olec.Interface.GTK
import Olec.Interface.Types
import Olec.Interface.Image
import Olec.Interface.Events

-- |
fillArea :: Style -> Char -> Visualiser
fillArea style c = do
	(w, h) <- ask
	alignVertically (hints w h)
	where
		line w = replicate w c
		hints w h = replicate h (Absolute 1 (drawString style (line w)))

newtype LayoutDelegate = LayoutDelegate (IORef (Position, Size))

-- |
newLayoutDelegate :: IO LayoutDelegate
newLayoutDelegate = LayoutDelegate <$> newIORef ((0, 0), (0, 0))

class Layout a where
	performLayout :: a -> Position -> Size -> IO ()

class (Layout a) => Widget a where
	paintWidget :: (Output o) => a -> o -> IO ()

instance Layout LayoutDelegate where
	performLayout (LayoutDelegate ref) origin size =
		writeIORef ref (origin, size)

data Handle a = Handle a LayoutDelegate

instance Layout (Handle a) where
	performLayout (Handle _ ld) = performLayout ld

instance (Visual a) => Widget (Handle a) where
	paintWidget (Handle w (LayoutDelegate ref)) out = do
		(origin, size) <- readIORef ref
		img <- runVisualiser size (visualize w)
		outputImage out origin img

-- |
newHandle :: a -> IO (Handle a)
newHandle w = Handle w <$> newLayoutDelegate

-- |
bindWidget :: (EventSource o, Output o, Widget a) => o -> a -> IO ()
bindWidget o w = do
	onResize o $ \ size -> do
		clearOutput o
		performLayout w (0, 0) size
		paintWidget w o

	size <- sizeOfOutput o

	clearOutput o
	performLayout w (0, 0) size
	paintWidget w o

data Test = Test

instance Visual Test where
	visualize _ = fillArea (Style (Color 255 255 255) (Color 255 255 255)) ' '

main :: IO ()
main = do
	o <- newInterface
	onKeyEvent o (\ _ -> True <$ exitInterface)

	h <- newHandle Test
	bindWidget o h

	runInterface
