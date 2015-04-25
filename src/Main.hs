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
mkDisplay :: Fd -> IO V.Vty
mkDisplay pts =
	V.mkVty mempty {
		V.inputFd = Just pts,
		V.outputFd = Just pts
	}

-- |
data RuntimeEnvironment w e = RuntimeEnvironment {
	reChan :: Chan (Event e),
	reDisplay :: V.Vty,
	reRenderer :: Renderer w
}

-- |
type Runtime w e = ReaderT (RuntimeEnvironment w e) (StateT (RenderContext w) IO)

-- |
data RuntimeEvent e
	= RKeyPress Word32 Word32
	| RUserEvent e
	| RExitRequest
	deriving (Show)

-- |
fetchEvent :: Runtime w e (RuntimeEvent e)
fetchEvent = do
	chan <- asks reChan

	let loop = do
		ev <- liftIO (readChan chan)
		case ev of
			ExitRequest ->
				return RExitRequest

			Resize width height -> do
				modify (\ rs -> rs {rcSize = (width, height)})
				render
				loop

			KeyPress k m ->
				return (RKeyPress k m)

			UserEvent e ->
				return (RUserEvent e)

	loop

-- |
requestExit :: Runtime w e ()
requestExit = liftIO mainQuit

-- |
render :: Runtime w e ()
render = do
	ctx <- get
	RuntimeEnvironment _ display renderer <- ask
	liftIO (V.update display (renderPicture renderer ctx))

-- |
evalRuntime :: (Visual w) => w -> Runtime w e a -> IO a
evalRuntime w m = do
	(chan, pts) <- makeInterface
	display <- mkDisplay pts

	evalStateT (runReaderT m (RuntimeEnvironment chan display mkRenderer)) (RenderContext (80, 24) w)

-- |
getState :: Runtime w e w
getState = gets rcState

-- |
putState :: w -> Runtime w e ()
putState w = modify (\ rc -> rc {rcState = w})

-- |
modifyState :: (w -> w) -> Runtime w e ()
modifyState f = modify (\ rc -> rc {rcState = f (rcState rc)})

data RootWidget = RootWidget

instance Visual RootWidget where
	mkRenderer =
		alignVertically [
			Relative 0.5 (fillChar mempty 'A'),
			LeftOver (fillChar mempty 'B')
		]

-- | Entry point
main :: IO ()
main =
	evalRuntime RootWidget $ do
		render
		void fetchEvent
