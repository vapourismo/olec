module Olec.Auxiliary.Metrics (
	DivisionHint (..),
	divideMetric
) where

import Data.Maybe
import Data.Traversable

data DivisionHint i f a
	= Absolute i a
	| Relative f a
	| LeftOver a

-- | Divide a list of items across a metric using the given hints.
divideMetric :: (Integral i, RealFrac f) => [DivisionHint i f a] -> i -> [(i, a)]
divideMetric elems total =
	filter (\ (n, _) -> n > 0) (catMaybes passedTwice)
	where
		-- Traverse through the list of input hints to convert
		-- relative factors to absolute metrics aswell and
		-- calculate the value which will be divided among
		-- the remaining "LeftOver" instances
		((left', unassigned'), passedOnce) = mapAccumL stepOne (total, 0) elems

		stepOne (left, unassigned) (Absolute i a)
			| i > 0 =
				let used = min left i
				in ((left - used, unassigned), Just (Left (used, a)))
		stepOne (left, unassigned) (Relative f a)
			| f > 0 =
				let used = min left (round (min f 1.0 * fromIntegral total))
				in ((left - used, unassigned), Just (Left (used, a)))
		stepOne (left, unassigned) (LeftOver a) =
			((left, unassigned + 1), Just (Right a))
		stepOne asis _ =
			(asis, Nothing)

		-- Figure out which value to give the "LeftOver" instances
		(normValue, lastValue) = let (d, m) = divMod left' unassigned' in (d, d + m)

		-- Traverse again to tag remaining "LeftOver" instances with
		-- the previously calculated value.
		(_, passedTwice) = mapAccumL stepTwo unassigned' (catMaybes passedOnce)

		stepTwo acc (Left x) = (acc, Just x)
		stepTwo n (Right _) | n <= 0 = (0, Nothing)
		stepTwo 1 (Right a) = (0, Just (lastValue, a))
		stepTwo n (Right a) = (n - 1, Just (normValue, a))
