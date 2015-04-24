{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main where

import Control.Concurrent

import Control.Monad.State
import Control.Monad.Reader

import Data.Word

import qualified Graphics.Vty as V
import Graphics.UI.Gtk (mainQuit)

import Olec.Events
import Olec.Interface
import Olec.Render

import System.Posix.Types

-- |
makeDisplay :: Fd -> IO V.Vty
makeDisplay pts =
	V.mkVty mempty {
		V.inputFd = Just pts,
		V.outputFd = Just pts
	}

-- |
data RuntimeEnvironment e = RuntimeEnvironment {
	reChan :: Chan (Event e),
	reDisplay :: V.Vty
}

-- |
data RuntimeState = RuntimeState {
	rsRenderer :: Renderer,
	rsSize :: Size
}

-- |
type Runtime e = ReaderT (RuntimeEnvironment e) (StateT RuntimeState IO)

-- |
data RuntimeEvent e
	= RKeyPress Word32 Word32
	| RUserEvent e
	| RExitRequest

-- |
fetchEvent :: Runtime e (RuntimeEvent e)
fetchEvent = do
	RuntimeEnvironment chan display <- ask

	let loop = do
		ev <- liftIO (readChan chan)
		case ev of
			KeyPress m k | m == toModifierMask [Control] &&
			               k == toKeyValue "q" -> do
				liftIO mainQuit
				return RExitRequest

			ExitRequest ->
				return RExitRequest

			Resize width height -> do
				vis <- gets rsRenderer
				liftIO (V.update display (renderPicture vis (width, height)))
				put (RuntimeState vis (width, height))
				loop

			KeyPress k m ->
				return (RKeyPress k m)

			UserEvent e ->
				return (RUserEvent e)

	loop

-- |
render :: Renderer -> Runtime e ()
render vis = do
	size <- gets rsSize
	display <- asks reDisplay
	liftIO (V.update display (renderPicture vis size))
	put (RuntimeState vis size)

-- |
evalRuntime :: Runtime e a -> IO a
evalRuntime m = do
	(chan, pts) <- makeInterface
	display <- makeDisplay pts

	evalStateT (runReaderT m (RuntimeEnvironment chan display)) (RuntimeState emptyRenderer (80, 24))

-- | Entry point
main :: IO ()
main = void $ evalRuntime $ do
	render $
		alignVertically [
			LeftOver (fillChar mempty 'A'),
			LeftOver (fillChar mempty 'B')
		]
	fetchEvent
