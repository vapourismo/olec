{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Concurrent
import Control.Monad.Reader

import Data.IORef
import Data.Metrics

import System.Random

import Olec.Interface

-- |
class Widget a where
	render :: a -> IO ()

	layout :: a -> Layout ()

-- |
data LayoutContext = LayoutContext {
	lcOrigin :: Position,
	lcSize   :: Size
}

-- |
toLayoutContext :: (Canvas c) => c -> IO LayoutContext
toLayoutContext canvas =
	LayoutContext <$> canvasOrigin canvas <*> canvasSize canvas

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
data DisplayDelegate = DisplayDelegate Display (IORef LayoutContext)

-- |
toDisplayDelegate :: Display -> IO DisplayDelegate
toDisplayDelegate display =
	DisplayDelegate display <$> (newIORef =<< toLayoutContext display)

instance Canvas DisplayDelegate where
	canvasSize (DisplayDelegate _ ref) =
		lcSize <$> readIORef ref

	canvasOrigin (DisplayDelegate _ ref) =
		lcOrigin <$> readIORef ref

	feedCanvas (DisplayDelegate display _) =
		feedCanvas display

-- |
delegateLayout :: DisplayDelegate -> Layout ()
delegateLayout (DisplayDelegate _ ref) =
	ask >>= liftIO . writeIORef ref

-- |
data Rainbow = Rainbow DisplayDelegate

-- |
newRainBow :: Display -> IO Rainbow
newRainBow display =
	Rainbow <$> toDisplayDelegate display

instance Widget Rainbow where
	render (Rainbow del) =
		runRenderer renderer del
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

	layout (Rainbow del) = delegateLayout del

-- |
data StaticWidget = StaticWidget DisplayDelegate (Renderer ())

-- |
newStaticWidget :: Display -> Renderer () -> IO StaticWidget
newStaticWidget display renderer =
	StaticWidget <$> toDisplayDelegate display <*> pure renderer

instance Widget StaticWidget where
	render (StaticWidget del renderer) =
		runRenderer renderer del

	layout (StaticWidget del _) =
		delegateLayout del

-- | Entry point
main :: IO ()
main = do
	(events, display) <- makeInterface

	rb <- newRainBow display
	fillA <- newStaticWidget display (resetStyle >> fillDrawingArea 'A')
	fillB <- newStaticWidget display (resetStyle >> fillDrawingArea 'B')

	let globalLayout = divideHoriz [Absolute 10 (layout fillA),
	                                Absolute 1 (pure ()),
	                                LeftOver (layout rb),
	                                Absolute 1 (pure ()),
	                                Absolute 10 (layout fillB)]
	runLayout globalLayout display

	render rb
	render fillA
	render fillB

	let loop = do
		e <- readChan events
		case e of
			Resize _ _ -> do
				clearDisplay display

				runLayout globalLayout display

				render rb
				render fillA
				render fillB

				loop

			_ -> pure ()

	loop
