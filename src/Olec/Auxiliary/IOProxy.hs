{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Olec.Auxiliary.IOProxy (
	-- * Base
	IOProxy,
	newIOProxy,
	proxyIOProxy,

	-- * Interaction
	readIOProxy,
	writeIOProxy,
	modifyIOProxy
) where

import Control.Lens

import Data.IORef
import Data.Tuple

-- | Proxy for an IORef
data IOProxy t
	= State (IORef t)
	| forall s. Proxy (IOProxy s) (Lens' s t)

-- | Create a new IOProxy.
newIOProxy :: s -> IO (IOProxy s)
newIOProxy = fmap State . newIORef

-- | Create proxy for another proxy.
proxyIOProxy :: IOProxy s -> Lens' s t -> IOProxy t
proxyIOProxy = Proxy

-- | Read underlying value using a lens.
readIOProxyLens :: IOProxy s -> Lens' s t -> IO t
readIOProxyLens (State ref) b =
	fmap (view b) (readIORef ref)
readIOProxyLens (Proxy proxy a) b =
	readIOProxyLens proxy (a . b)

-- | Read underlying value.
readIOProxy :: IOProxy s -> IO s
readIOProxy (State ref) =
	readIORef ref
readIOProxy (Proxy proxy a) =
	readIOProxyLens proxy a

-- | Change the underlying value using a lens.
writeIOProxyLens :: IOProxy s -> Lens' s t -> t -> IO ()
writeIOProxyLens (State ref) b value =
	atomicModifyIORef' ref (\ parent -> (set b value parent, ()))
writeIOProxyLens (Proxy proxy a) b value =
	writeIOProxyLens proxy (a . b) value

-- | Change the underlying value.
writeIOProxy :: IOProxy s -> s -> IO ()
writeIOProxy (State ref) value =
	atomicWriteIORef ref (seq value value)
writeIOProxy (Proxy proxy a) value =
	writeIOProxyLens proxy a value

-- | Modify the underlying value using a lens.
modifyIOProxyLens :: IOProxy s -> Lens' s t -> (t -> (a, t)) -> IO a
modifyIOProxyLens (State ref) b f =
	atomicModifyIORef' ref $ \ parent ->
		let (x, value') = f (view b parent)
		in (set b value' parent, x)
modifyIOProxyLens (Proxy proxy a) b f =
	modifyIOProxyLens proxy (a . b) f

-- | Modify the underlying value.
modifyIOProxy :: IOProxy s -> (s -> (a, s)) -> IO a
modifyIOProxy (State ref) f =
	atomicModifyIORef' ref (\ parent -> swap (f parent))
modifyIOProxy (Proxy proxy a) f =
	modifyIOProxyLens proxy a f
