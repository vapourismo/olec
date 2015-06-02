{-# LANGUAGE DeriveFunctor #-}

module Data.Metrics (
	DivisionHint (..),
	divideMetric
) where

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
		((left', unassigned'), passedOnce) = mapAccumL stepOne (total, 0) elems

		stepOne (left, unassigned) (Absolute i a)
			| i > 0 =
				let used = min left i
				in ((left - used, unassigned), Left (used, a))
		stepOne (left, unassigned) (Relative f a)
			| f > 0 =
				let used = min left (round (min f 1.0 * fromIntegral total))
				in ((left - used, unassigned), Left (used, a))
		stepOne (left, unassigned) (LeftOver a) =
			((left, unassigned + 1), Right a)
		stepOne asis x =
			(asis, Left (0, cnt x))

		cnt (Absolute _ x) = x
		cnt (Relative _ x) = x
		cnt (LeftOver x) = x

		-- Figure out which value to give the "LeftOver" instances
		(normValue, lastValue) = let (d, m) = divMod left' unassigned' in (d, d + m)

		-- Traverse again to tag remaining "LeftOver" instances with
		-- the previously calculated value.
		(_, passedTwice) = mapAccumL stepTwo unassigned' passedOnce

		stepTwo n (Left x)           = (n,     x             )
		stepTwo n (Right x) | n <= 0 = (0,     (0,         x))
		stepTwo 1 (Right x)          = (0,     (lastValue, x))
		stepTwo n (Right x)          = (n - 1, (normValue, x))
