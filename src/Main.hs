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

	render :: a -> Renderer ()

	layout :: a -> Layout ()

-- |
data LayoutContext = LayoutContext Position Size

-- |
type Layout = ReaderT LayoutContext IO

-- |
runLayout :: (Canvas c) => Layout a -> c -> IO a
runLayout lay canvas =
	LayoutContext <$> canvasOrigin canvas
	              <*> canvasSize canvas
	              >>= runReaderT lay

-- |
divideHoriz :: [DivisionHint Int Float (Layout ())] -> Layout ()
divideHoriz hints = do
	LayoutContext _ (w, _) <- ask
	make 0 (divideMetric hints w)

	where
		make _ [] = pure ()
		make offset ((elemWidth, lay) : xs) = do
			withReaderT (\ (LayoutContext (_, y) (_, h)) ->
			                 LayoutContext (offset, y) (elemWidth, h))
			            lay
			make (offset + elemWidth) xs

-- |
data Rainbow = Rainbow

instance Component Rainbow where
	data Setup Rainbow = SetupRainbow

	newComponent _ _ = pure Rainbow

	render _ = do
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

	layout _ = pure ()

-- | Entry point
main :: IO ()
main = do
	(events, display) <- makeInterface

	rb <- newComponent display SetupRainbow
	runRenderer (render rb) display

	let loop = do
		e <- readChan events
		case e of
			Resize _ _ -> do
				runLayout (layout rb) display
				runRenderer (render rb) display
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
