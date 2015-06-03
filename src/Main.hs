{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.IORef
import Data.Tuple
import Data.Metrics

import qualified Data.Text as T

import Olec.Interface

-- | Component
class Component r s where
	-- | Perform layout
	layoutRT :: Runtime r s Layout ()

	-- | Paint stuff
	paintRT :: Runtime r s IO ()

-- | Component which may be used by the root UI facilities
class (Component r s) => RootComponent r s where
	-- | Before UI lauched
	startRT :: Runtime r s IO ()

	-- | After UI exited
	exitRT :: Runtime r s IO ()

	-- | On key events
	inputRT :: KeyEvent -> Runtime r s IO Bool

-- | Handke to a Component
data Handle r s = Handle {
	hStatic   :: r,
	hDynamic  :: IORef s,
	hDelegate :: LayoutDelegate
}

instance (Component r s) => Widget (Handle r s) where
	layout h = do
		delegateLayout (hDelegate h)
		withComponent h layoutRT

	paint h = withComponent h paintRT

instance (RootComponent r s) => RootWidget (Handle r s) where
	data Setup (Handle r s) = ConstructRootWidget (Construct (r, s))

	setup d (ConstructRootWidget c) = do
		h <- runReaderT (c >>= uncurry newHandle) d
		withComponent h startRT
		pure h

	exit h = withComponent h exitRT

	input h e = withComponent h (inputRT e)

-- | Component Runtime
newtype Runtime r s m a = Runtime { runM :: ReaderT (Handle r s) m a }
	deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (Monad m) => MonadReader r (Runtime r s m) where
	ask = Runtime (reader hStatic)

	local f (Runtime r) = Runtime (local (\ h -> h {hStatic = f (hStatic h)}) r)

instance (MonadIO m) => MonadState s (Runtime r s m) where
	get = Runtime (reader hDynamic >>= liftIO . readIORef)

	put x = Runtime (reader hDynamic >>= liftIO . flip writeIORef x)

	state f = Runtime (reader hDynamic >>= liftIO . flip atomicModifyIORef' (swap . f))

-- | Do something with a Component.
withComponent :: Handle r s -> Runtime r s m a -> m a
withComponent h (Runtime r) = runReaderT r h

-- | ???
shareRT :: (Monad n) => Runtime r s m a -> Runtime r s n (m a)
shareRT m = flip withComponent m <$> Runtime ask

-- | Render something.
renderRT :: (MonadIO m) => Renderer a -> Runtime r s m a
renderRT r = Runtime ask >>= liftIO . runRenderer r . hDelegate

-- | Use this when constructing Components.
type Construct = ReaderT Display IO

-- | Construct a Component and return its Handle.
newHandle :: r -> s -> Construct (Handle r s)
newHandle r s =
	Handle r <$> liftIO (newIORef s)
	         <*> (ask >>= liftIO . toLayoutDelegate)

data StatusBarConfig = StatusBarConfig Color Color

data StatusBarText = StatusBarText T.Text

instance Component StatusBarConfig StatusBarText where
	layoutRT = pure ()

	paintRT = do
		StatusBarConfig fg bg <- ask
		StatusBarText mode <- get

		renderRT $ do
			resetStyle
			fillDrawingArea ' '
			divideVert [LeftOver   (pure ()),
			            Absolute 1 (drawBar fg bg mode)]
			resetCursor

		where
			drawBar fg bg mode = do
				setForegroundColor fg
				setBackgroundColor bg

				fillDrawingArea ' '

				divideHoriz [Absolute 1 (pure ()),
				             LeftOver   (drawMode mode),
				             Absolute 1 (pure ())]

			drawMode mode = do
				setForegroundColor (Color 255 0 0)
				setBackgroundColor (Color 0 0 0)

				drawString " "
				drawText mode
				drawString " "

instance RootComponent StatusBarConfig StatusBarText where
	startRT = pure ()

	exitRT = pure ()

	inputRT _ = lift exitUI >> pure True

-- | Entry point
main :: IO ()
main = launchUI $ ConstructRootWidget $
	pure (StatusBarConfig (Color 0 0 0) (Color 255 255 255),
	      StatusBarText "StatusBarText Bar Text")
