{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader

import Data.Time
import Data.IORef
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
data Clock = Clock (IORef UTCTime)

instance Visual Clock where
	visualize (Clock ref) = do
		tm <- liftIO (readIORef ref)
		let text = formatTime defaultTimeLocale "%T" tm

		(w, _) <- getSize
		let result =
			divideMetric [LeftOver () :: DivisionHint Int Float (),
			              Absolute (stringWidth text) (),
			              LeftOver ()] w

		case result of
			[]                           -> pure()
			(m, _) : []                  -> drawStrings 0 m 0 text
			(m, _) : (r, _) : []         -> drawStrings 0 m r text
			(l, _) : (m, _) : (r, _) : _ -> drawStrings l m r text

		where
			drawStrings l m r text = do
				drawString (replicate l ' ')
				drawString (fitString m text)
				drawString (replicate r ' ')

-- |
newClock :: Display -> IO (FlatWidget Clock)
newClock d = do
	clockState <- newIORef =<< getCurrentTime
	clock <- newFlatWidget d (Clock clockState)

	forkIO $ forever $ do
		threadDelay 1000000
		tm <- getCurrentTime
		writeIORef clockState tm
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
	(eventChan, display) <- makeInterface

	rb <- newRootWidget display
	glueWidget display rb

	readChan eventChan
	pure ()
