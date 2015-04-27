module Olec.Runtime (
	-- * Runtime basics
	Runtime,
	RuntimeEvent (..),
	evalRuntime,
	evalRuntime_,

	-- * Events
	fetchEvent,
	requestExit,

	-- * Render
	render,

	-- * State
	getState,
	putState,
	modifyState,

	-- * Modules
	module Olec.Events,
	module Olec.Render
) where

import Control.Exception
import Control.Concurrent

import Control.Monad.State
import Control.Monad.Reader

import Data.Word

import qualified Graphics.Vty as V
import Graphics.UI.Gtk (mainQuit)

import System.Posix.Types

import Olec.Events
import Olec.Interface
import Olec.Render

-- | Create a Vty instance.
makeDisplay :: Fd -> IO V.Vty
makeDisplay pts =
	V.mkVty mempty {
		V.inputFd = Just pts,
		V.outputFd = Just pts
	}

-- | Information relevant to several runtime functions.
data RuntimeEnvironment w e =
	REnv (Chan (Event e)) V.Vty (Renderer w)

-- | An action which represents the flow of the underlying program.
type Runtime w e =
	ReaderT (RuntimeEnvironment w e) (StateT (RenderContext w) IO)

-- | An event which occurs during runtime.
data RuntimeEvent e
	= RKeyPress Word32 Word32
	| RUserEvent e
	| RExitRequest
	deriving (Show, Eq, Ord)

-- | Evaluate the runtime action.
evalRuntime :: Runtime w e a -> Renderer w -> w -> IO a
evalRuntime m r w = do
	(chan, pts) <- makeInterface
	display <- makeDisplay pts
	evalStateT (runReaderT m (REnv chan display r)) (RenderContext (80, 24) w)

-- | Evaluate the runtime action, but discard the actual value.
evalRuntime_ :: Runtime w e a -> Renderer w -> w -> IO ()
evalRuntime_ m r w =
	void (evalRuntime m r w) `catch` \ BlockedIndefinitelyOnMVar -> return ()

-- | Grab the next event.
fetchEvent :: Runtime w e (RuntimeEvent e)
fetchEvent = do
	REnv chan _ _ <- ask

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

-- | Request the termination of this program. The program does not exit immediately,
--   instead you have to handle the "RExitRequest" event, which will be sent
--   when the main loop has exited.
requestExit :: Runtime w e ()
requestExit = liftIO mainQuit

-- | Render the current state to the screen.
render :: Runtime w e ()
render = do
	ctx <- get
	REnv _ display renderer <- ask
	liftIO (V.update display (renderPicture renderer ctx))

-- | Get the user state.
getState :: Runtime w e w
getState = gets rcState

-- | Overwrite the user state.
putState :: w -> Runtime w e ()
putState w = modify (\ rc -> rc {rcState = w})

-- | Modify the user state.
modifyState :: (w -> w) -> Runtime w e ()
modifyState f = modify (\ rc -> rc {rcState = f (rcState rc)})
