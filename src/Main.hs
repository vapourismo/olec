{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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

	newComponent :: Position -> Size -> Setup a -> IO a

	requestRedraw :: a -> (B.ByteString -> IO ()) -> IO ()

	requestResize :: a -> Position -> Size -> IO ()

-- |
data VisualConstraint = VisualConstraint {
	visOrigin :: Position,
	visSize   :: Size
}

-- |
type Layout = ReaderT VisualConstraint IO

-- |
runLayout :: Layout a -> Position -> Size -> IO a
runLayout l p s = runReaderT l (VisualConstraint p s)

-- |
fitComponent :: (Component a) => a -> Layout ()
fitComponent com = do
	VisualConstraint origin size <- ask
	liftIO (requestResize com origin size)

-- |
divideHoriz :: [DivisionHint Int Float (Layout ())] -> Layout ()
divideHoriz hints = do
	VisualConstraint _ (w, _) <- ask
	sequence_ (snd (mapAccumL mapper 0 (divideMetric hints w)))

	where
		moveLayout offset elemWidth (VisualConstraint (x0, y0) (_, h)) =
			VisualConstraint (x0 + offset, y0) (elemWidth, h)

		mapper offset (elemWidth, layout) =
			(offset + elemWidth, withReaderT (moveLayout offset elemWidth) layout)

-- |
data Rainbow = Rainbow (IORef (Size, Position))

instance Component Rainbow where
	data Setup Rainbow = SetupRainbow

	newComponent origin size _ =
		Rainbow <$> newIORef (origin, size)

	requestRedraw (Rainbow ref) output = do
		(origin, size) <- readIORef ref
		runRenderer renderer output origin size
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

	requestResize (Rainbow ref) origin size =
		writeIORef ref (origin, size)

-- | Entry point
main :: IO ()
main = do
	(events, feedIO, sizeIO) <- makeInterface

	size <- sizeIO
	rb <- newComponent (0, 0) size SetupRainbow
	requestRedraw rb feedIO

	let loop = do
		e <- readChan events
		case e of
			Resize width height -> do
				requestResize rb (0, 0) (width, height)
				requestRedraw rb feedIO
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
