{-# LANGUAGE OverloadedStrings #-}

module Olec.Application (
	terminalApplication
) where

import Control.Monad
import Control.Concurrent

import Graphics.Vty hiding (Event)

import System.Posix.Types

import Olec.Events

-- | The function name is just a coincidence.
terminalApplication :: Fd -> Chan Event -> IO ()
terminalApplication pts events = do
	vty <- mkVty mempty {
		inputFd = Just pts,
		outputFd = Just pts
	}

	forever $ do
		ev <- readChan events
		update vty Picture {
			picCursor = NoCursor,
			picLayers = [string mempty (show ev)],
			picBackground = ClearBackground
		}
