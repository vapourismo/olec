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
import Olec.Interface.Display

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
type Layout = ReaderT LayoutContext IO

-- | Perform the "Layout" actions.
runLayout :: (Canvas c) => Layout a -> c -> IO a
runLayout lay canvas =
	LayoutContext <$> canvasOrigin canvas
	              <*> canvasSize canvas
	              >>= runReaderT lay

-- | Get the absolute origin.
layoutOrigin :: Layout Position
layoutOrigin = asks lcOrigin

-- | Get the layout size.
layoutSize :: Layout Size
layoutSize = asks lcSize

-- | Divide the layout horizontally. For more information look at "divideMetric".
divideHoriz :: [DivisionHint Int Float (Layout ())] -> Layout ()
divideHoriz hints = do
	LayoutContext _ (w, _) <- ask
	make 0 (divideMetric hints w)

	where
		make _ [] = pure ()
		make offset ((elemWidth, lay) : xs) = do
			withReaderT (\ (LayoutContext (_, y) (_, h)) ->
			                 LayoutContext (offset, y) (elemWidth, h))
			            lay
			make (offset + elemWidth) xs

-- | Divide the layout vertically. For more information look at "divideMetric".
divideVert :: [DivisionHint Int Float (Layout ())] -> Layout ()
divideVert hints = do
	LayoutContext _ (_, h) <- ask
	make 0 (divideMetric hints h)

	where
		make _ [] = pure ()
		make offset ((elemHeight, lay) : xs) = do
			withReaderT (\ (LayoutContext (x, _) (w, _)) ->
			                 LayoutContext (x, offset) (w, elemHeight))
			            lay
			make (offset + elemHeight) xs

-- | A "Canvas" used as a restricted proxy to the main "Display"
data LayoutDelegate = LayoutDelegate Display (IORef LayoutContext)

instance Canvas LayoutDelegate where
	canvasSize (LayoutDelegate _ ref) =
		lcSize <$> readIORef ref

	canvasOrigin (LayoutDelegate _ ref) =
		lcOrigin <$> readIORef ref

	feedCanvas (LayoutDelegate display _) =
		feedCanvas display

-- | Construct a "LayoutDelege" using a "Display".
--   The delegate targets the entire "Canvas" area, initially.
toLayoutDelegate :: Display -> IO LayoutDelegate
toLayoutDelegate display =
	LayoutDelegate display <$> (newIORef =<< toLayoutContext display)

-- | Submit the received "LayoutContext" to the delegate.
delegateLayout :: LayoutDelegate -> Layout ()
delegateLayout (LayoutDelegate _ ref) =
	ask >>= liftIO . writeIORef ref
