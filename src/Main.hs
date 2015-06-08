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
import Data.Metrics
import Data.Handle

import qualified Data.Text as T

import Olec.Event.Keys
import Olec.Interface.GTK

import Olec.Visual
import Olec.Visual.Slot
import Olec.Visual.Layout

class (LayoutElement a) => Widget a where
	paint :: a -> IO ()

data Flat s = Flat Slot s

instance LayoutElement (Flat s) where
	layout (Flat slot _) = layout slot

instance (Paintable s) => Widget (Flat s) where
	paint (Flat slot st) =
		paintSlot slot (toPainter st)

type FlatIORef s = Flat (IORef s)

newFlat :: (Canvas o) => o -> s -> IO (Flat (IORef s))
newFlat out s =
	Flat <$> toSlot out <*> newIORef s

newtype FlatT s a = FlatT (ReaderT (Flat s) IO a)
	deriving (Functor, Applicative, Monad, MonadIO)

instance MonadReader s (FlatT s) where
	ask = FlatT (ReaderT (\ (Flat _ s) -> pure s))

	local f (FlatT (ReaderT g)) =
		FlatT (ReaderT (\ (Flat slot r) -> g (Flat slot (f r))))

instance (Handle h) => MonadState s (FlatT (h s)) where
	get = ask >>= liftIO . readHandle

	put x = ask >>= liftIO . flip writeHandle x

	state f = ask >>= liftIO . flip modifyHandle (swap . f)

runFlatT :: FlatT s a -> Flat s -> IO a
runFlatT (FlatT r) = runReaderT r

render :: (Paintable s) => FlatT s ()
render = FlatT (ReaderT (\ (Flat slot s) -> paintSlot slot (toPainter s)))

-- |
data Test = Test T.Text

instance Paintable Test where
	toPainter (Test txt) =
		center (text (Style "#000000" "#ffffff") txt)

-- |
data Global = Global (FlatIORef Test)

instance LayoutElement Global where
	layout (Global test) =
		vlayout [LeftOver (pure ()), Absolute 1 (layout test)]

instance Widget Global where
	paint (Global test) =
		paint test

-- |
newGlobal :: (Canvas o) => o -> IO Global
newGlobal out =
	Global <$> newFlat out (Test "Hello World")

-- |
registerRootWidget :: (Widget w) => Interface -> w -> IO ()
registerRootWidget iface w =  do
	registerResizeHandler iface $ \ size -> do
		clearCanvas iface
		runLayout (0, 0) size (layout w)
		paint w

	size <- sizeOfCanvas iface
	runLayout (0, 0) size (layout w)
	paint w

-- | Entry Point
main :: IO ()
main = do
	iface <- newInterface

	w <- newGlobal iface

	-- Keys
	src <- newChan
	registerKeyHandler iface (writeChan src)

	let km = bindKeys $
		bind "Control-q" (const exitInterface)

	forkIO (forever (handleKeyEvent km () src))

	-- Visual
	registerRootWidget iface w

	runInterface
