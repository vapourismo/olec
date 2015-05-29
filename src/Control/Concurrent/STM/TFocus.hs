{-# LANGUAGE Rank2Types #-}

module Control.Concurrent.STM.TFocus (
	TFocus,
	newTFocus,
	newTFocusIO,
	toTFocus,
	moveTFocus,
	readTFocus,
	writeTFocus,
	modifyTFocus
) where

import Control.Lens
import Control.Monad.STM
import Control.Concurrent.STM.TVar

-- | Focus on a particular part of a value inside a "TVar"
data TFocus s t
	= TFocus (TVar s) (Lens' s t)

-- | Create a new "TFocus".
newTFocus :: s -> STM (TFocus s s)
newTFocus v = toTFocus <$> newTVar v

-- | Create a new "TFocus".
newTFocusIO :: s -> IO (TFocus s s)
newTFocusIO v = toTFocus <$> newTVarIO v

-- | Lift a "TVar" into a "TFocus".
toTFocus :: TVar s -> TFocus s s
toTFocus ref = TFocus ref (iso id id)

-- | Create a new "TFocus" which focuses deeper.
moveTFocus :: TFocus s t -> Lens' t u -> TFocus s u
moveTFocus (TFocus ref a) b = TFocus ref (a . b)

-- | Retrieve the focused item.
readTFocus :: TFocus s t -> STM t
readTFocus (TFocus ref a) = view a <$> readTVar ref

-- | Replace the focused item.
writeTFocus :: TFocus s t -> t -> STM ()
writeTFocus (TFocus ref a) v = modifyTVar' ref (set a v)

-- | Modify the focused item.
modifyTFocus :: TFocus s t -> (t -> t) -> STM ()
modifyTFocus (TFocus ref a) f = modifyTVar' ref (over a f)
