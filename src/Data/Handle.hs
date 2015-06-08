module Data.Handle (
	Handle (..),
	NonEmptyMVar,
) where

import Control.Concurrent.MVar

import Data.IORef

-- | A handle to a shared state.
class Handle h where
	newHandle :: a -> IO (h a)

	readHandle :: h a -> IO a

	writeHandle :: h a -> a -> IO ()

	modifyHandle :: h a -> (a -> (a, b)) -> IO b

instance Handle MVar where
	newHandle = newMVar

	readHandle = readMVar

	writeHandle m x = modifyMVar_ m (const (pure x))

	modifyHandle m f = modifyMVar m (pure . f)

instance Handle IORef where
	newHandle = newIORef

	readHandle = readIORef

	writeHandle = writeIORef

	modifyHandle = atomicModifyIORef'

-- | An MVar which is never empty.
newtype NonEmptyMVar a = NonEmptyMVar (MVar a)

instance Handle NonEmptyMVar where
	newHandle x = NonEmptyMVar <$> newHandle x

	readHandle (NonEmptyMVar m) = readHandle m

	writeHandle (NonEmptyMVar m) = writeHandle m

	modifyHandle (NonEmptyMVar m) = modifyHandle m
