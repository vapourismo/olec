{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader

import System.Random

import Olec.Interface

class Visual a where
	visualize :: a -> Renderer ()

class Container a where
	layout :: a -> Layout ()

class (Container a, Visual a) => Widget a where
	paint :: a -> IO ()

-- |
update :: (Container a, Widget a, Canvas c) => a -> c -> IO ()
update w c = do
	runLayout (layout w) c
	paint w

-- |
data Handle a where
	Handle    :: a -> LayoutDelegate -> Handle a
	ContainerHandle :: (Container a) => a -> LayoutDelegate -> Handle a

-- |
newWidget :: Display -> a -> IO (Handle a)
newWidget d w =
	Handle w <$> toLayoutDelegate d

-- |
newContainer :: (Container a) => Display -> a -> IO (Handle a)
newContainer d w =
	ContainerHandle w <$> toLayoutDelegate d

instance Container (Handle a) where
	layout (Handle _ del) = delegateLayout del
	layout (ContainerHandle w del) = delegateLayout del >> layout w

instance (Visual a) => Visual (Handle a) where
	visualize (Handle w _) = visualize w
	visualize (ContainerHandle w _) = visualize w

instance (Visual a) => Widget (Handle a) where
	paint (Handle w del) = runRenderer (visualize w) del
	paint (ContainerHandle w del) = runRenderer (visualize w) del

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

	update rb display

	let loop = do
		e <- readChan events
		case e of
			Resize _ _ -> do
				clearDisplay display

				update rb display

				loop

			_ -> pure ()

	loop
