{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Olec.Interface.Layout (
	-- * Context
	LayoutContext (..),
	toLayoutContext,

	-- * Layout
	Layout,
	runLayout,
	layoutOrigin,
	layoutSize,
	divideHoriz,
	divideVert,

	-- * Delegate
	LayoutDelegate,
	toLayoutDelegate,
	delegateLayout
) where

import Control.Monad.Reader

import Data.IORef
import Data.Metrics

import Olec.Interface.Types
import Olec.Interface.Renderer

-- | Captures layout restrictions
data LayoutContext = LayoutContext {
	lcOrigin :: Position,
	lcSize   :: Size
}

-- | Construct a "LayoutContext" using a "Canvas" snapshot.
--   The layout context targets the entire "Canvas" area, initially.
toLayoutContext :: (Canvas c) => c -> IO LayoutContext
toLayoutContext canvas =
	LayoutContext <$> canvasOrigin canvas <*> canvasSize canvas

-- | Layout actions
newtype Layout a = Layout (ReaderT LayoutContext IO a)
	deriving (Functor, Applicative, Monad, MonadReader LayoutContext, MonadIO)

-- | Perform the "Layout" actions.
runLayout :: (Canvas c) => Layout a -> c -> IO a
runLayout (Layout lay) canvas =
	LayoutContext <$> canvasOrigin canvas
	              <*> canvasSize canvas
	              >>= runReaderT lay

-- | Get the absolute origin.
layoutOrigin :: Layout Position
layoutOrigin = asks lcOrigin

-- | Get the layout size.
layoutSize :: Layout Size
layoutSize = asks lcSize

-- | Is the underlying LayoutContext adjustable?
class Placeable m where
	askPlaceable :: m LayoutContext

	withPlaceable :: (LayoutContext -> LayoutContext) -> m a -> m a

instance Placeable Layout where
	askPlaceable = ask

	withPlaceable = local

instance Placeable Renderer where
	askPlaceable = LayoutContext (0, 0) <$> getSize

	withPlaceable f r = do
		LayoutContext origin size <- f . LayoutContext (0, 0) <$> getSize
		constrainRenderer origin size r

-- | Divide the layout horizontally. For more information look at "divideMetric".
divideHoriz :: (Monad m, Placeable m) => [DivisionHint Int Float (m ())] -> m ()
divideHoriz hints = do
	LayoutContext _ (w, _) <- askPlaceable
	make 0 (divideMetric hints w)
	where
		make _ [] = pure ()
		make offset ((elemWidth, lay) : xs) = do
			withPlaceable
				(\ (LayoutContext (_, y) (_, h)) -> LayoutContext (offset, y) (elemWidth, h))
				lay
			make (offset + elemWidth) xs

-- | Divide the layout vertically. For more information look at "divideMetric".
divideVert :: [DivisionHint Int Float (Layout ())] -> Layout ()
divideVert hints = do
	LayoutContext _ (_, h) <- askPlaceable
	make 0 (divideMetric hints h)
	where
		make _ [] = pure ()
		make offset ((elemHeight, lay) : xs) = do
			withPlaceable
				(\ (LayoutContext (x, _) (w, _)) -> LayoutContext (x, offset) (w, elemHeight))
				lay
			make (offset + elemHeight) xs

-- | A "Canvas" used as a restricted proxy to a "Canvas"
data LayoutDelegate = forall c . (Canvas c) => LayoutDelegate c (IORef LayoutContext)

instance Canvas LayoutDelegate where
	canvasSize (LayoutDelegate _ ref) =
		lcSize <$> readIORef ref

	canvasOrigin (LayoutDelegate _ ref) =
		lcOrigin <$> readIORef ref

	feedCanvas (LayoutDelegate canvas _) =
		feedCanvas canvas

-- | Construct a "LayoutDelege" using a "Canvas".
--   The delegate targets the entire "Canvas" area, initially.
toLayoutDelegate :: (Canvas c) => c -> IO LayoutDelegate
toLayoutDelegate canvas =
	LayoutDelegate canvas <$> (newIORef =<< toLayoutContext canvas)

-- | Submit the received "LayoutContext" to the delegate.
delegateLayout :: LayoutDelegate -> Layout ()
delegateLayout (LayoutDelegate _ ref) =
	ask >>= liftIO . writeIORef ref
