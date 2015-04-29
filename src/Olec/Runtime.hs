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

-- |
data Manifest s = Manifest {
	mfChannel  :: Chan Event,
	mfDisplay  :: MVar Vty.Vty,
	mfSize     :: IO Size,
	mfStateRef :: IORef s,
	mfRenderer :: Renderer s
}

-- |
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

-- |
run :: Runtime s a -> Renderer s -> s -> IO a
run runtime renderer initState = do
	(events, display, size) <- makeInterface
	stateRef <- newIORef initState
	displayVar <- newMVar display
	evalRuntime runtime (Manifest events displayVar size stateRef renderer)

-- |
forkRuntime :: Runtime s () -> Runtime s ThreadId
forkRuntime (Runtime f) = Runtime (forkIO . f)

-- |
render :: Runtime s ()
render =
	Runtime $ \ Manifest {..} ->
		renderPicture mfRenderer <$> (RenderContext <$> mfSize
		                                            <*> readIORef mfStateRef)
		                         >>= withMVar mfDisplay . flip Vty.update

-- |
requestExit :: Runtime s ()
requestExit = liftIO GTK.mainQuit

-- |
fetchEvent :: Runtime s Event
fetchEvent = Runtime (\ mf -> readChan (mfChannel mf))
