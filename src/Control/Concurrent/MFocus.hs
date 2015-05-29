{-# LANGUAGE Rank2Types, ExistentialQuantification #-}

module Control.Concurrent.MFocus (
	MFocus,
	newMFocus,
	toMFocus,
	moveMFocus,
	readMFocus,
	writeMFocus,
	modifyMFocus
) where

import Control.Lens
import Control.Concurrent.MVar

-- | Focus on a particular part of a value inside a "MVar"
data MFocus s =
	forall o. MFocus (MVar o) (Lens' o s)

-- | Create a new "MFocus".
newMFocus :: s -> IO (MFocus s)
newMFocus v = toMFocus <$> newMVar v

-- | Lift a "MVar" into a "MFocus".
toMFocus :: MVar s -> MFocus s
toMFocus ref = MFocus ref (iso id id)

-- | Create a new "MFocus" which focuses deeper.
moveMFocus :: MFocus t -> Lens' t u -> MFocus u
moveMFocus (MFocus ref a) b = MFocus ref (a . b)

-- | Retrieve the focused item.
readMFocus :: MFocus t -> IO t
readMFocus (MFocus ref a) = view a <$> readMVar ref

-- | Replace the focused item.
writeMFocus :: MFocus t -> t -> IO ()
writeMFocus (MFocus ref a) v = modifyMVar_ ref (pure . set a v)

-- | Modify the focused item.
modifyMFocus :: MFocus t -> (t -> t) -> IO ()
modifyMFocus (MFocus ref a) f = modifyMVar_ ref (pure . over a f)
