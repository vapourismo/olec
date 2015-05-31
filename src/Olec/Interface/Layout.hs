module Olec.Interface.Layout (
	-- * Context
	LayoutContext (..),
	toLayoutContext,

	-- * Layout
	Layout,
	runLayout,

	layoutOrigin,
	layoutSize,

	divideHoriz
) where

import Control.Monad.Reader

import Data.Metrics

import Olec.Interface.Types
import Olec.Interface.Renderer

-- | Captures layout restrictions
data LayoutContext = LayoutContext {
	lcOrigin :: Position,
	lcSize   :: Size
}

-- | Construct a "LayoutContext" using a "Canvas" snapshot.
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
