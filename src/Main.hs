{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Control.Monad
import Control.Monad.Reader

import Data.Time
import Data.IORef
import Data.Handle
import Data.Metrics

import qualified Data.Text as T

import Olec.Event.Keys
import Olec.Interface.GTK

import Olec.Visual
import Olec.Visual.Slot
import Olec.Visual.Layout
import Olec.Visual.Widget

-- |
data StatusBar = StatusBar

instance Paintable StatusBar where
	toPainter StatusBar =
		center (text (Style "#ff0000" "#00ffff") "StatusBar")

-- |
data WorkspaceNavigator = WorkspaceNavigator

instance Paintable WorkspaceNavigator where
	toPainter WorkspaceNavigator =
		text (Style "#ff0000" "#00ffff") "WorkspaceNavigator"

-- |
data EditorPane = EditorPane

instance Paintable EditorPane where
	toPainter EditorPane =
		text (Style "#ff0000" "#00ffff") "EditorPane"

-- |
data RootWidget =
	RootWidget
		(Flat (IORef StatusBar))
		(Flat (IORef WorkspaceNavigator))
		(Flat (IORef EditorPane))

instance LayoutElement RootWidget where
	layout (RootWidget sb wn ep) =
		vlayout [LeftOver (hlayout [Absolute 40 (layout wn),
		                            LeftOver (layout ep)]),
		         Absolute 1 (layout sb)]

instance Widget RootWidget where
	paint (RootWidget sb wn ep) = do
		paint sb
		paint wn
		paint ep

-- |
newRootWidget :: (Canvas o) => o -> IO (RootWidget)
newRootWidget out =
	RootWidget <$> newFlatHandle out StatusBar
	           <*> newFlatHandle out WorkspaceNavigator
	           <*> newFlatHandle out EditorPane

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

	-- Keys
	src <- newChan
	registerKeyHandler iface (writeChan src)

	let km = bindKeys $
		bind "Control-q" (const exitInterface)

	forkIO (forever (handleKeyEvent km () src))

	-- Visual
	rw <- newRootWidget iface
	registerRootWidget iface rw

	runInterface
