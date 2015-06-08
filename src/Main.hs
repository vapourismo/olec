{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent

import Control.Monad
import Control.Monad.Reader

import Data.Time
import Data.IORef
import Data.Metrics

import qualified Data.Text as T

import Olec.Event.Keys
import Olec.Interface.GTK

import Olec.Visual
import Olec.Visual.Layout
import Olec.Visual.Widget

-- |
data StatusBar = StatusBar T.Text

instance Paintable StatusBar where
	toPainter (StatusBar txt) = do
		tm <- liftIO getCurrentTime
		let clockString = formatTime defaultTimeLocale "%T" tm
		layered [text (Style "#000000" "#ffffff") txt,
		         justifyRight (string (Style "#000000" "#ffffff") clockString)]

refreshDelayed :: (Widget w) => Int -> w -> IO ()
refreshDelayed n w =
	forever $ do
		threadDelay n
		paint w

-- |
data Global = Global (Flat (IORef StatusBar))

instance LayoutElement Global where
	layout (Global test) =
		vlayout [LeftOver (pure ()), Absolute 1 (layout test)]

instance Widget Global where
	paint (Global test) =
		paint test

-- |
newGlobal :: (Canvas o) => o -> IO Global
newGlobal out = do
	sb <- newFlat out (StatusBar "Hello World")
	forkIO (refreshDelayed 1000000 sb)
	pure (Global sb)

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
