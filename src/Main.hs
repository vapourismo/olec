{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader

import Data.Metrics
import Data.IORef

import System.Random

import Olec.Interface

class Visual a where
	visualize :: a -> Renderer ()

class Container a where
	layout :: a -> Layout ()

data Widget a = Widget a LayoutDelegate

-- |
newWidget :: Display -> a -> IO (Widget a)
newWidget d w =
	Widget w <$> toLayoutDelegate d

-- |
useContainer :: (Container a) => Widget a -> Layout ()
useContainer (Widget w del) = do
	delegateLayout del
	layout w

-- |
useWidget :: Widget a -> Layout ()
useWidget (Widget _ del) =
	delegateLayout del

-- |
updateContainer :: (Container a, Canvas c) => Widget a -> c -> IO ()
updateContainer cont =
	runLayout (useContainer cont)

-- |
updateWidget :: (Canvas c) => Widget a -> c -> IO ()
updateWidget widget =
	runLayout (useWidget widget)

-- |
paintWidget :: (Visual a) => Widget a -> IO ()
paintWidget (Widget w del) =
	runRenderer (visualize w) del

-- |
data Rainbow = Rainbow

instance Visual Rainbow where
	visualize _ = do
		(width, height) <- getSize
		forM_ [0 .. height - 1] $ \ y -> do
			moveCursor 0 y
			forM_ [0 .. width - 1] $ \ _ -> do
				fg <- liftIO (Color <$> randomIO <*> randomIO <*> randomIO)
				bg <- liftIO (Color <$> randomIO <*> randomIO <*> randomIO)

				setForegroundColor fg
				setBackgroundColor bg

				c <- liftIO (randomRIO ('A', 'Z'))
				drawString [c]

-- | Entry point
main :: IO ()
main = do
	(events, display) <- makeInterface

	rb <- newWidget display Rainbow

	updateWidget rb display
	paintWidget rb

	let loop = do
		e <- readChan events
		case e of
			Resize _ _ -> do
				clearDisplay display

				updateWidget rb display
				paintWidget rb

				loop

			_ -> pure ()

	loop
