{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader

import Data.List
import Data.IORef
import Data.Metrics

import qualified Data.ByteString as B

import System.IO
import System.Random

import Olec.Interface

-- |
class Component a where
	data Setup a

	newComponent :: (Canvas c) => c -> Setup a -> IO a

	paintComponent :: (Canvas c) => a -> c -> IO ()

	updateComponent :: (Canvas c) => a -> c -> IO ()

---- |
--type Layout c = ReaderT c IO

---- |
--runLayout :: (Canvas c) => Layout c a -> c -> IO a
--runLayout = runReaderT

---- |
--fitComponent :: (Component a, Canvas c) => a -> Layout c ()
--fitComponent com =
--	ask >>= liftIO . updateComponent com

---- |
--divideHoriz :: (Canvas c) => [DivisionHint Int Float (Layout c ())] -> Layout c ()
--divideHoriz hints = do
--	canvas <- ask
--	(w, _) <- liftIO (canvasSize canvas)
--	make 0 (divideMetric hints w)

--	where
--		make _ [] = pure ()
--		make offset ((elemWidth, ReaderT layout) : xs) = do
--			ReaderT $ \ canvas -> do
--				(_, y) <- canvasOrigin canvas
--				(_, h) <- canvasSize canvas
--				layout ()
--			make (offset + elemWidth) xs

-- |
data Rainbow = Rainbow

instance Component Rainbow where
	data Setup Rainbow = SetupRainbow

	newComponent _ _ = pure Rainbow

	paintComponent _ canvas =
		runRenderer renderer canvas
		where
			renderer = do
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

	updateComponent _ _ = pure ()

-- | Entry point
main :: IO ()
main = do
	(events, display) <- makeInterface

	rb <- newComponent display SetupRainbow
	paintComponent rb display

	let loop = do
		e <- readChan events
		case e of
			Resize _ _ -> do
				updateComponent rb display
				paintComponent rb display
				loop

			_ -> pure ()

	loop

	where
		--renderer1 = do
		--	moveCursor 10 10
		--	setForegroundColor (Color 255 0 0)
		--	setBackgroundColor (Color 255 255 255)
		--	drawString "Herro"

		--	moveCursor 10 11
		--	setForegroundColor (Color 255 255 255)
		--	setBackgroundColor (Color 255 0 0)
		--	drawString "Werld"

		--renderer2 = do
		--	(width, height) <- getSize
		--	forM_ [0 .. height - 1] $ \ y -> do
		--		moveCursor 0 y
		--		forM_ [0 .. width - 1] $ \ _ -> do
		--			fg <- liftIO (Color <$> randomIO <*> randomIO <*> randomIO)
		--			bg <- liftIO (Color <$> randomIO <*> randomIO <*> randomIO)

		--			setForegroundColor fg
		--			setBackgroundColor bg

		--			c <- liftIO (randomRIO ('A', 'Z'))
		--			drawString [c]
