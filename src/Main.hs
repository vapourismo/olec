{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader

import Data.Time
import Data.Metrics

import System.Random

import Olec.Interface

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

-- |
data Clock = Clock

instance Visual Clock where
	visualize _ =
		formatTime defaultTimeLocale "%T"
			<$> liftIO getCurrentTime
			>>= drawString

-- |
newClock :: Display -> IO (FlatWidget Clock)
newClock d = do
	clock <- newFlatWidget d Clock
	forkIO $ forever $ do
		threadDelay 1000000
		paint clock
	pure clock

-- |
data RootWidget = RootWidget (FlatWidget Rainbow) (FlatWidget Clock)

instance Widget RootWidget where
	layout (RootWidget rb clock) =
		divideVert [LeftOver (layout rb), Absolute 1 (layout clock)]

	paint (RootWidget rb clock) = do
		paint rb
		paint clock

-- |
newRootWidget :: Display -> IO RootWidget
newRootWidget d =
	RootWidget <$> newFlatWidget d Rainbow <*> newClock d

-- | Entry point
main :: IO ()
main = do
	(eventChan, resizeVar, display) <- makeInterface

	rb <- newRootWidget display

	update rb display

	forkIO $ forever $ do
		takeMVar resizeVar
		clearDisplay display
		update rb display

	readChan eventChan
	pure ()
