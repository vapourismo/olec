{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.IORef
import Data.Tuple

import qualified Data.Text as T

import Olec.Event.Keys
import Olec.Interface.GTK

import Olec.Visual
import Olec.Visual.Slot
import Olec.Visual.Layout

data Flat s = Flat Slot s

instance LayoutElement (Flat s) where
	layout (Flat slot _) = layout slot

newFlatIORef :: (Canvas o) => o -> s -> IO (Flat (IORef s))
newFlatIORef out s =
	Flat <$> toSlot out <*> newIORef s

paintWidget :: (Paintable s) => Flat s -> IO ()
paintWidget (Flat slot st) =
	paintSlot slot (toPainter st)

newtype FlatT r s a = FlatT (ReaderT (Flat (r s)) IO a)
	deriving (Functor, Applicative, Monad, MonadReader (Flat (r s)), MonadIO)

instance MonadState s (FlatT IORef s) where
	get = ask >>= \ (Flat _ ref) -> liftIO (readIORef ref)

	put x = ask >>= \ (Flat _ ref) -> liftIO (writeIORef ref x)

	state f = ask >>= \ (Flat _ ref) -> liftIO (atomicModifyIORef' ref (swap . f))

runFlatT :: Flat (r s) -> FlatT r s a -> IO a
runFlatT w (FlatT r) = runReaderT r w

paint :: (Paintable (r s)) => FlatT r s ()
paint = ask >>= \ (Flat slot s) -> liftIO (paintSlot slot (toPainter s))

-- |
data Test = Test T.Text

instance Paintable Test where
	toPainter (Test txt) =
		center (text (Style "#000000" "#ffffff") txt)

-- | Entry Point
main :: IO ()
main = do
	iface <- newInterface

	w <- newFlatIORef iface (Test "Hello World")

	-- Keys
	src <- newChan
	registerKeyHandler iface (writeChan src)

	let km = bindKeys $
		bind "Control-q" (const exitInterface)

	forkIO (forever (handleKeyEvent km () src))

	-- Visual
	registerResizeHandler iface $ \ size -> do
		clearCanvas iface
		runLayout (0, 0) size (layout w)
		paintWidget w

	runInterface
