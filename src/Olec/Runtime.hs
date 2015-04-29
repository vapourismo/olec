{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, RecordWildCards #-}

module Olec.Runtime (
	-- * Runtime basics
	Runtime,
	run,
	forkRuntime,

	-- * Events
	fetchEvent,
	requestExit,

	-- * Render
	render,

	-- * Exports
	ask,
	get,
	put,
	modify,
	liftIO,

	module Olec.Events,
	module Olec.Render
) where

import Control.Concurrent

import Control.Monad.State
import Control.Monad.Reader

import Data.IORef
import Data.Tuple

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.Gtk as GTK

import Olec.Render
import Olec.Interface
import Olec.Events

-- | Runtime manifest
data Manifest s = Manifest {
	mfChannel  :: Chan Event,
	mfDisplay  :: MVar Vty.Vty,
	mfSize     :: IO Size,
	mfStateRef :: IORef s,
	mfRenderer :: Renderer s
}

-- | Cow
newtype Runtime s a = Runtime { evalRuntime :: Manifest s -> IO a }

instance Functor (Runtime s) where
	fmap f (Runtime g) = Runtime (fmap f . g)

instance Applicative (Runtime s) where
	pure = Runtime . const . pure
	Runtime f <*> Runtime g = Runtime (\ mf -> f mf <*> g mf)

instance Monad (Runtime s) where
	Runtime f >>= g = Runtime (\ mf -> f mf >>= \ x -> evalRuntime (g x) mf)

instance MonadReader Event (Runtime s) where
	ask = Runtime (readChan . mfChannel)
	local _ = id

instance MonadState s (Runtime s) where
	get = Runtime (readIORef . mfStateRef)
	state f = Runtime (\ mf -> atomicModifyIORef (mfStateRef mf) (swap . f))
	put s = Runtime (\ mf -> writeIORef (mfStateRef mf) s)

instance MonadIO (Runtime s) where
	liftIO = Runtime . const

-- | Execute runtime with a renderer and initial state.
run :: Runtime s a -> Renderer s -> s -> IO a
run runtime renderer initState = do
	(events, display, size) <- makeInterface
	stateRef <- newIORef initState
	displayVar <- newMVar display
	evalRuntime runtime (Manifest events displayVar size stateRef renderer)

-- | Fork a thread to run another runtime in.
forkRuntime :: Runtime s () -> Runtime s ThreadId
forkRuntime (Runtime f) = Runtime (forkIO . f)

-- | Render the current state.
render :: Runtime s ()
render =
	Runtime $ \ Manifest {..} ->
		renderPicture mfRenderer <$> (RenderContext <$> mfSize
		                                            <*> readIORef mfStateRef)
		                         >>= withMVar mfDisplay . flip Vty.update

-- | Request the main loop to exit.
requestExit :: Runtime s ()
requestExit = liftIO GTK.mainQuit

-- | Get an event from the main loop.
fetchEvent :: Runtime s Event
fetchEvent = Runtime (\ mf -> readChan (mfChannel mf))
