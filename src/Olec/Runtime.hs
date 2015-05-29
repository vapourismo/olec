{-# LANGUAGE ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances,
             RecordWildCards,
             Rank2Types #-}

module Olec.Runtime (
	-- * Runtime
	Runtime,
	RemoteRuntime (..),
	Manifest (..),
	run,
	withRuntime,
	forkRuntime,
	terminateRemoteRuntime,

	-- * Events
	requestExit,
	ask,
	forwardEvent,

	-- -- * Render
	-- render,

	-- * State
	get,
	put,
	modify,

	-- * Re-exports
	liftIO,
	module Olec.Interface.Events,
	module Olec.Render
) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TFocus

import Control.Monad.State.Strict
import Control.Monad.Reader

import Control.Lens

import qualified Graphics.Vty as Vty
import qualified Graphics.UI.Gtk as GTK

import Olec.Interface
import Olec.Interface.Events
import Olec.Render

-- | Runtime manifest
data Manifest s = Manifest {
	mfChannel  :: Chan Event,
	mfStateRef :: TFocus s,
	mfRenderer :: IO ()
}

-- | A forked runtime.
data RemoteRuntime = RemoteRuntime (Chan Event) ThreadId

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
	get = Runtime (readTFocusIO . mfStateRef)
	state f = Runtime $ \ Manifest {..} -> do
		r <- atomically $ do
			v <- readTFocus mfStateRef
			let (r, v') = f v
			writeTFocus mfStateRef v'
			pure r
		mfRenderer
		pure r
	put s = Runtime (\ Manifest {..} -> atomically (writeTFocus mfStateRef s) <* mfRenderer)

instance MonadIO (Runtime s) where
	liftIO = Runtime . const

-- | Execute runtime with a renderer and initial state.
run :: Runtime s a -> Renderer s -> s -> IO a
run runtime renderer initState = do
	(events, display, size) <- makeInterface
	stateRef <- newTFocusIO $! initState

	displayVar <- newMVar $! display
	requestsVar <- newTVarIO 0

	evalRuntime runtime (Manifest events
	                              stateRef
	                              (renderIO size
	                                        displayVar
	                                        (readTFocusIO stateRef)
	                                        requestsVar
	                                        renderer))

-- | Delegate a runtime to a component of the original state.
withRuntime :: Lens' s t -> Runtime t a -> Runtime s a
withRuntime b (Runtime rt) =
	Runtime (\ mf -> rt (mf {mfStateRef = moveTFocus (mfStateRef mf) b}))

-- | Fork a thread to run another runtime in.
forkRuntime :: Runtime s () -> Runtime s RemoteRuntime
forkRuntime (Runtime rt) =
	Runtime $ \ mf -> do
		sepChan <- newChan
		tid <- forkIO (rt mf {mfChannel = sepChan})
		pure (RemoteRuntime sepChan tid)

-- | Kill a remote runtime.
terminateRemoteRuntime :: RemoteRuntime -> Runtime s ()
terminateRemoteRuntime (RemoteRuntime _ tid) =
	liftIO (throwTo tid ThreadKilled)

-- | Forward an event to a seperate forked "Manifest".
forwardEvent :: Event -> RemoteRuntime -> Runtime s ()
forwardEvent ev (RemoteRuntime chan _) =
	liftIO (writeChan chan ev)

-- | Render IO action
renderIO :: IO Size
         -> MVar Vty.Vty
         -> IO s
         -> TVar Word
         -> Renderer s
         -> IO ()
renderIO grSize grDisplay grState grRequests grRenderer = do
	atomically (modifyTVar' grRequests (+ 1))
	withMVar grDisplay $ \ display -> do
		counter <- atomically (readTVar grRequests <* writeTVar grRequests 0)
		when (counter > 0) $
			RenderContext <$> grSize
			              <*> grState
			              >>= Vty.update display . renderPicture grRenderer


-- | Request the main loop to exit.
requestExit :: Runtime s ()
requestExit = liftIO GTK.mainQuit
