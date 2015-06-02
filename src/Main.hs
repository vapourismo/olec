{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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
		let [(l, _), (m, _), (r, _)] =
			divideMetric [LeftOver () :: DivisionHint Int Float (),
			              Absolute (stringWidth text) (),
			              LeftOver ()] w

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
data MyRootWidget = MyRootWidget (FlatWidget Rainbow) (FlatWidget Clock)

instance Widget MyRootWidget where
	layout (MyRootWidget rb clock) =
		divideVert [LeftOver (layout rb), Absolute 1 (layout clock)]

	paint (MyRootWidget rb clock) = do
		paint rb
		paint clock

instance RootWidget MyRootWidget where
	data Setup MyRootWidget = SetupMyRootWidget

	input _ _ = do
		exitUI
		pure False

	exit _ = putStrLn "Bye"

	setup d _ = MyRootWidget <$> newFlatWidget d Rainbow <*> newClock d

-- | Entry point
main :: IO ()
main = launchUI SetupMyRootWidget
