module Olec.Terminal.Actions (
	Action (..),
	getSize,
	getCursor,
	moveCursor,
	drawString,
	drawByteString,
	runAction,
	runAction',
	T.withTerminal,
	T.refresh
) where

import Foreign.C.Types

import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.ByteString as B

import qualified Olec.Terminal.Bindings as T


type TargetDetails = (CInt, CInt, CInt, CInt)

newtype Action a = Action (TargetDetails -> IO a)


instance Functor Action where
	fmap f (Action g) = Action (fmap f . g)

instance Applicative Action where
	pure = Action . const .  return
	Action f <*> Action g = Action (\origin -> f origin <*> g origin)
	Action f <* Action g = Action (\origin -> f origin <* g origin)
	Action f *> Action g = Action (\origin -> f origin *> g origin)

instance Monad Action where
	return = pure
	(>>) = (*>)
	Action f >>= g = Action (\origin -> f origin >>= \r -> let Action h = g r in h origin)

instance MonadIO Action where
	liftIO = Action . const


-- |
getSize :: Action (CInt, CInt)
getSize = Action (\(x, y, maxx, maxy) -> return (maxx - x, maxy - y))


-- |
getCursor :: Action (CInt, CInt)
getCursor = Action (\(x, y, _, _) -> (\(curX, curY) -> (curX - x, curY - y)) <$> T.getCursor)

-- |
moveCursor :: (CInt, CInt) -> Action ()
moveCursor (curx, cury) = Action (\(x, y, maxx, maxy) -> T.moveCursor (max x (min maxx curx), max y (min maxy cury)))


-- |
drawString :: String -> Action ()
drawString = liftIO . T.drawString

-- |
drawByteString :: B.ByteString -> Action ()
drawByteString = liftIO . T.drawByteString


-- |
runAction :: TargetDetails -> Action a -> IO a
runAction origin (Action f) = f origin

-- |
runAction' :: Action a -> IO a
runAction' (Action f) = T.maxXY >>= \(maxx, maxy) -> f (0, 0, maxx, maxy)
