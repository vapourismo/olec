module Olec.Visual.Layout (
	Layout,
	LayoutElement (..),
	runLayout,
	vlayout,
	hlayout
) where

import Control.Monad.Reader

import Data.Metrics

import Olec.Visual.Types

-- | Layout
type Layout = ReaderT (Position, Size) IO ()

-- | Execute layout actions
runLayout :: Position -> Size -> Layout -> IO ()
runLayout o s l = runReaderT l (o, s)

-- | Type which have an inner Layout structure
class LayoutElement a where
	layout :: a -> Layout

-- | Align elements vertically.
vlayout :: [DivisionHint Int Float Layout] -> Layout
vlayout hints = do
	((_, y), (_, h)) <- ask
	make y (divideMetric hints h)
	where
		make :: Int -> [(Int, Layout)] -> ReaderT (Position, Size) IO ()
		make _ [] = pure ()
		make y ((h, l) : ls) = do
			local (\ ((x, _), (w, _)) -> ((x, y), (w, h))) l
			make (y + h) ls

-- | Align elements horizontally.
hlayout :: [DivisionHint Int Float Layout] -> Layout
hlayout hints = do
	((x, _), (w, _)) <- ask
	make x (divideMetric hints w)
	where
		make :: Int -> [(Int, Layout)] -> ReaderT (Position, Size) IO ()
		make _ [] = pure ()
		make x ((w, l) : ls) = do
			local (\ ((_, y), (_, h)) -> ((x, y), (w, h))) l
			make (x + w) ls
