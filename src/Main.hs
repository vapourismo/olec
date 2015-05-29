{-# LANGUAGE OverloadedStrings #-}

module Main where

import Olec.Runtime

-- | Entry point
main :: IO ()
main =
	run runtime ()
	where
		runtime = pure ()
