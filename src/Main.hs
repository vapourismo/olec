{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.State.Strict

import qualified Data.Map.Strict as M

import System.Timeout

import Olec.Event.Keys
import Olec.Interface.GTK

-- |
data KeyMap a
	= KeyHandler (a -> KeySource -> IO Bool)
	| KeyMap (M.Map KeyEvent (KeyMap a))

-- |
type KeySource = Chan KeyEvent

-- |
handleKeyEvent :: KeyMap a -> a -> KeySource -> IO Bool
handleKeyEvent (KeyHandler handle) x src =
	handle x src
handleKeyEvent (KeyMap km) x src = do
	ev <- readChan src
	maybe (pure False) handleKeyEvent2 (M.lookup ev km)
	where
		handleKeyEvent2 (KeyHandler handle) =
			handle x src
		handleKeyEvent2 (KeyMap km2) = do
			mbEvent <- timeout 2000000 (readChan src)
			maybe (pure False) handleKeyEvent2 (mbEvent >>= flip M.lookup km2)

-- |
type KeyBinder a = State (M.Map KeyEvent (KeyMap a)) ()

bindKeys :: KeyBinder a -> KeyMap a
bindKeys b = KeyMap (execState b M.empty)

-- |
bind :: KeyEvent -> (a -> IO ()) -> KeyBinder a
bind ev handle =
	modify (M.insert ev (KeyHandler (\ x _ -> True <$ handle x)))

-- |
nest :: KeyEvent -> KeyBinder a -> KeyBinder a
nest ev (StateT f) =
	StateT $ \ m ->
		fmap (\ nm -> M.insertWith merge ev (KeyMap nm) m) <$> f M.empty
	where
		merge (KeyMap a) (KeyMap b) = KeyMap (M.unionWith merge a b)
		merge _ x = x

-- | Entry Point
main :: IO ()
main = do
	iface <- newInterface

	src <- newChan
	registerKeyHandler iface (writeChan src)

	let km = bindKeys $ do
		bind "control-q" (\ () -> exitInterface)
		nest "control-x" $ do
			bind "control-c" (\ () -> putStrLn "control-x control-c")
			bind "control-v" (\ () -> putStrLn "control-x control-v")

	forkIO (forever (handleKeyEvent km () src >>= print))

	runInterface
