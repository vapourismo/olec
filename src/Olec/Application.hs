{-# LANGUAGE OverloadedStrings #-}

module Olec.Application (
	terminalApplication
) where

import Control.Concurrent

import System.Posix.Types

import Olec.Events
import Olec.Render

-- |
newtype EventLogWidget = EventLogWidget [Event]

instance Visual EventLogWidget where
	render (EventLogWidget events) (width, height) =
		cropRight width $ vertCat $
			map (string mempty . show) (reverse (take height events))

-- |
appendEvent :: EventLogWidget -> Event -> EventLogWidget
appendEvent (EventLogWidget evs) ev = EventLogWidget (ev : evs)

-- | The function name is just a coincidence.
terminalApplication :: Fd -> Chan Event -> IO ()
terminalApplication pts events = do
	vty <- mkVty mempty {
		inputFd = Just pts,
		outputFd = Just pts
	}

	let loop w s = do
		ev <- readChan events
		case ev of
			KeyPress m k | m == toModifierMask [Control] &&
			               k == toKeyValue "q" ->
				return ()
			Resize x y -> loop w (x, y)
			_ -> do
				let w' = appendEvent w ev
				update vty Picture {
					picCursor = NoCursor,
					picLayers = [render w' s],
					picBackground = ClearBackground
				}
				loop w' s

	loop (EventLogWidget []) (1, 1)
