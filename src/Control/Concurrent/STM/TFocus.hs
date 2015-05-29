{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module Control.Concurrent.STM.TFocus (
	TFocus,
	newTFocus,
	newTFocusIO,
	toTFocus,
	moveTFocus,
	readTFocus,
	readTFocusIO,
	writeTFocus,
	modifyTFocus
) where

import Control.Lens
import Control.Monad.STM
import Control.Concurrent.STM.TVar

-- | Focus on a particular part of a value inside a "TVar"
data TFocus s =
	forall o. TFocus (TVar o) (Lens' o s)

-- | Create a new "TFocus".
newTFocus :: s -> STM (TFocus s)
newTFocus v = toTFocus <$> newTVar v

-- | Create a new "TFocus".
newTFocusIO :: s -> IO (TFocus s)
newTFocusIO v = toTFocus <$> newTVarIO v

-- | Lift a "TVar" into a "TFocus".
toTFocus :: TVar s -> TFocus s
toTFocus ref = TFocus ref (iso id id)

-- | Create a new "TFocus" which focuses deeper.
moveTFocus :: TFocus t -> Lens' t u -> TFocus u
moveTFocus (TFocus ref a) b = TFocus ref (a . b)

-- | Retrieve the focused item.
readTFocus :: TFocus t -> STM t
readTFocus (TFocus ref a) = view a <$> readTVar ref

-- | Retrieve the focused item.
readTFocusIO :: TFocus t -> IO t
readTFocusIO (TFocus ref a) = view a <$> readTVarIO ref

-- | Replace the focused item.
writeTFocus :: TFocus t -> t -> STM ()
writeTFocus (TFocus ref a) v = modifyTVar' ref (set a v)

-- | Modify the focused item.
modifyTFocus :: TFocus t -> (t -> t) -> STM ()
modifyTFocus (TFocus ref a) f = modifyTVar' ref (over a f)
