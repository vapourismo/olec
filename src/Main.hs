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

import Olec.Interface

-- | Component
class Component r s where
	layoutRT :: Runtime r s Layout ()

	paintRT :: Runtime r s IO ()

-- | Component which may be used by the root UI facilities
class (Component r s) => RootComponent r s where
	startRT :: Runtime r s IO ()

	exitRT :: Runtime r s IO ()

	inputRT :: KeyEvent -> Runtime r s IO Bool

-- | Handke to a Component
data Handle r s = Handle {
	hStatic   :: r,
	hDynamic  :: IORef s,
	hDelegate :: LayoutDelegate
}

instance (Component r s) => Widget (Handle r s) where
	layout h = withComponent h layoutRT

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

data ClockR = ClockR

data ClockS = ClockS Word Word

instance Component ClockR ClockS where
	layoutRT = pure ()

	paintRT = do
		ClockS n r <- get
		renderRT (drawString (show (n, r)))
		put (ClockS n (r + 1))

instance RootComponent ClockR ClockS where
	startRT =
		liftIO (putStrLn "Handleello")

	exitRT =
		liftIO (putStrLn "Good bye")

	inputRT e = do
		ClockS n r <- get
		liftIO (print e)
		put (ClockS (n + 1) r)
		paintRT
		pure True

-- | Entry point
main :: IO ()
main = launchUI (ConstructRootWidget (pure (ClockR, ClockS 0 0)))
