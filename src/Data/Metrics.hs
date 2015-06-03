{-# LANGUAGE DeriveFunctor #-}

module Data.Metrics (
	DivisionHint (..),
	divideMetric,
	divideMetricFitted
) where

import Control.Arrow

import Data.Traversable

data DivisionHint i f a
	= Absolute i a
	| Relative f a
	| LeftOver a
	deriving (Functor)

-- | Divide a list of items across a metric using the given hints.
divideMetric :: (Integral i, RealFrac f)
             => [DivisionHint i f a] -- ^ Items to distribute
             -> i                    -- ^ Metric to divide
             -> [(i, a)]
divideMetric elems total =
	filter (\ (n, _) -> n > 0) passedTwice
	where
		-- Traverse through the list of input hints to convert
		-- relative factors to absolute metrics aswell and
		-- calculate the value which will be divided among
		-- the remaining "LeftOver" instances
		((space', unassigned'), passedOnce) = mapAccumL stepOne (total, 0) elems

		stepOne (space, unassigned) (Absolute i a)
			| i > 0 =
				let used = min space i
				in ((space - used, unassigned), Left (used, a))
		stepOne (space, unassigned) (Relative f a)
			| f > 0 =
				let used = min space (round (min f 1.0 * fromIntegral total))
				in ((space - used, unassigned), Left (used, a))
		stepOne (space, unassigned) (LeftOver a) =
			((space, unassigned + 1), Right a)
		stepOne asis x =
			(asis, Left (0, cnt x))

		cnt (Absolute _ x) = x
		cnt (Relative _ x) = x
		cnt (LeftOver x) = x

		-- Figure out which value to give the "LeftOver" instances
		(normValue, lastValue) = let (d, m) = divMod space' unassigned' in (d, d + m)

		-- Traverse again to tag remaining "LeftOver" instances with
		-- the previously calculated value.
		(_, passedTwice) = mapAccumL stepTwo unassigned' passedOnce

		stepTwo n (Left x)           = (n,     x             )
		stepTwo n (Right x) | n <= 0 = (0,     (0,         x))
		stepTwo 1 (Right x)          = (0,     (lastValue, x))
		stepTwo n (Right x)          = (n - 1, (normValue, x))

-- | Divide a list of items across a metric using the given hints
--   and a fitting function which calculates the actual used value.
divideMetricFitted :: (Monad m, Integral i, RealFrac f)
                   => [DivisionHint i f a] -- ^ Items to distribute
                   -> i                    -- ^ Metric to divide
                   -> ((i, a) -> m (i, b)) -- ^ Fitting function
                   -> m (i, [b])
divideMetricFitted elems total f =
	uncurry (mapAccumLM stepTwo) =<< mapAccumLM stepOne total elems
	where
		mapAccumLM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
		mapAccumLM _ acc [] = pure (acc, [])
		mapAccumLM it acc (x : xs) = do
			(acc', y) <- it acc x
			fmap (second (y :)) (mapAccumLM it acc' xs)

		-- Traverse through the list of input hints to convert
		-- relative factors to absolute metrics aswell and
		-- calculate the value which will be divided among
		-- the remaining "LeftOver" instances
		stepOne space (Absolute i y) =
			fmap ((space -) *** Left)
			     (f (min space (max 0 i), y))
		stepOne space (Relative r y) =
			let i = round (min (max 0 r) 1.0 * fromIntegral total)
			in fmap ((space -) *** Left)
			        (f (min space i, y))
		stepOne space (LeftOver a) =
			pure (space, Right a)

		-- Traverse again to tag remaining "LeftOver" instances with
		-- the previously calculated value.
		stepTwo space (Left x)  = pure (space, x)
		stepTwo space (Right y) = fmap (first (space -)) (f (space, y))
