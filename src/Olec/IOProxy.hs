{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module Olec.IOProxy (
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

-- | Proxy for an IORef
data IOProxy s
	= State (IORef s)
	| forall a. Proxy (IOProxy a) (Lens' a s)

-- | Create a new IOProxy.
newIOProxy :: s -> IO (IOProxy s)
newIOProxy = fmap State . newIORef

-- | Create proxy for another proxy.
proxyIOProxy :: IOProxy a -> Lens' a t -> IOProxy t
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
modifyIOProxyLens :: IOProxy s -> Lens' s t -> (t -> t) -> IO ()
modifyIOProxyLens (State ref) b f =
	atomicModifyIORef' ref (\ parent -> (over b f parent, ()))
modifyIOProxyLens (Proxy proxy a) b f =
	modifyIOProxyLens proxy (a . b) f

-- | Modify the underlying value.
modifyIOProxy :: IOProxy s -> (s -> s) -> IO ()
modifyIOProxy (State ref) f =
	atomicModifyIORef' ref (\ parent -> (f parent, ()))
modifyIOProxy (Proxy proxy a) f =
	modifyIOProxyLens proxy a f
