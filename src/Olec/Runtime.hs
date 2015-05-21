{-# LANGUAGE ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances,
             RecordWildCards,
             Rank2Types #-}

module Olec.Runtime (
	-- * Runtime
	Runtime,
	Manifest (..),
	run,
	withRuntime,
	forkRuntime,

	-- * Events
	requestExit,
	ask,
	forwardEvent,

	-- * Render
	render,

	-- * State
	get,
	put,
	modify,

	-- * Re-exports
	liftIO,
	module Olec.Interface.Events,
	module Olec.Render
) where

import Control.Concurrent

import Control.Monad.State
import Control.Monad.Reader

import Control.Lens

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.Gtk as GTK

import Olec.Interface
import Olec.Interface.Events
import Olec.Auxiliary.IOProxy
import Olec.Render

-- | Responsible for rendering the main stage.
data GlobalRenderer =
	forall s. GlobalRenderer (IO Size) (IOProxy s) (Renderer s)

-- | Runtime manifest
data Manifest s = Manifest {
	mfChannel  :: Chan Event,
	mfDisplay  :: MVar Vty.Vty,
	mfStateRef :: IOProxy s,
	mfRenderer :: GlobalRenderer
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
	get = Runtime (readIOProxy . mfStateRef)
	state f = Runtime (\ mf -> modifyIOProxy (mfStateRef mf) f)
	put s = Runtime (\ mf -> writeIOProxy (mfStateRef mf) s)

instance MonadIO (Runtime s) where
	liftIO = Runtime . const

-- | Execute runtime with a renderer and initial state.
run :: Runtime s a -> Renderer s -> s -> IO a
run runtime renderer initState = do
	(events, display, size) <- makeInterface
	stateRef <- newIOProxy initState
	displayVar <- newMVar display
	evalRuntime runtime (Manifest events displayVar stateRef (GlobalRenderer size stateRef renderer))

-- | Delegate a runtime to a component of the original state.
withRuntime :: Lens' s t -> Runtime t a -> Runtime s a
withRuntime b (Runtime rt) =
	Runtime (\ mf -> rt (mf {mfStateRef = proxyIOProxy (mfStateRef mf) b}))

-- | Fork a thread to run another runtime in.
forkRuntime :: Runtime s () -> Runtime s (Manifest s, ThreadId)
forkRuntime (Runtime rt) =
	Runtime $ \ mf -> do
		sepChan <- newChan
		let mf' = mf {mfChannel = sepChan}
		tid <- forkIO (rt mf)
		pure (mf', tid)

-- | Forward an event to a seperate forked "Manifest".
forwardEvent :: Event -> Manifest a -> Runtime s ()
forwardEvent ev Manifest {..} =
	liftIO (writeChan mfChannel ev)

-- | Render the current state.
render :: Runtime s ()
render =
	Runtime $ \ Manifest {mfRenderer = GlobalRenderer s p r, ..} ->
		renderPicture r <$> (RenderContext <$> s
		                                   <*> readIOProxy p)
		                >>= withMVar mfDisplay . flip Vty.update

-- | Request the main loop to exit.
requestExit :: Runtime s ()
requestExit = liftIO GTK.mainQuit
