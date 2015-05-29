{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}

module Olec.Render (
	-- * Base types
	Size,
	Position,

	-- * Render types
	RenderContext (..),
	RenderM,
	Renderer,
	renderPicture,
	renderImage,
	withRenderer,

	-- * Misc render functions
	getCanvasSize,
	getRenderState,

	-- * Cursor functions
	putCursor,
	getCursor,
	hideCursor,

	-- * Drawing
	emptyRenderer,
	drawText,
	drawString,
	fillChar,

	-- * Image
	imageHeight,
	imageWidth,

	-- * Layouts
	DivisionHint (..),
	alignVertically,
	alignHorizontally,

	-- * Attributes
	Attr,
	mkAttr,

	-- * Visuals
	Visual (..),
	withVisual
) where

import Control.Monad.State
import Control.Monad.Reader

import Control.Lens

import Data.Word
import qualified Data.Text as T

import Graphics.Vty hiding (Event, hideCursor)

import Olec.Auxiliary.Metrics

type Size = (Int, Int)

type Position = (Int, Int)

-- | Render context
data RenderContext w = RenderContext {
	rcSize :: Size,
	rcState :: w
}

-- | Render environment
type RenderM w = ReaderT (RenderContext w) (State Cursor)

-- | Image renderer
type Renderer w = RenderM w Image

-- | Run the renderer in a canvas with the given size.
renderPicture :: Renderer w -> RenderContext w -> Picture
renderPicture r c =
	Picture cur [img] ClearBackground
	where (img, cur) = runState (runReaderT r c) NoCursor

-- | Run the renderer but retrieve the image only.
renderImage :: Renderer w -> RenderContext w -> Image
renderImage r c@(RenderContext (w, h) _) = resize w h (evalState (runReaderT r c) NoCursor)

-- | Redirect to a another component.
withRenderer :: Lens' w t -> Renderer t -> Renderer w
withRenderer b =
	withReaderT (\ rc -> rc {rcState = view b (rcState rc)})

-- | Move the cursor to a certain location.
putCursor :: Position -> RenderM w ()
putCursor = put . uncurry Cursor

-- | Get the current cursor position.
getCursor :: RenderM w Cursor
getCursor = get

-- | Hide the cursor.
hideCursor :: RenderM w ()
hideCursor = put NoCursor

-- | Get the current canvas size.
getCanvasSize :: RenderM w Size
getCanvasSize = asks rcSize

-- | Get the render target.
getRenderState :: RenderM w w
getRenderState = asks rcState

-- | An empty image.
emptyRenderer :: Renderer w
emptyRenderer = return emptyImage

-- | Produce an image containing the given "Text".
drawText :: Attr -> T.Text -> Renderer w
drawText attr val = pure (text' attr val)

-- | Produce an image containing the given "String".
drawString :: Attr -> String -> Renderer w
drawString attr val = pure (string attr val)

-- | Fill the canvas using the given "Char".
fillChar :: Attr -> Char -> Renderer w
fillChar attr val = do
	(w, h) <- getCanvasSize
	fmap vertCat (replicateM h (drawString attr (replicate w val)))

-- | Align elements vertically.
alignVertically :: [DivisionHint Int Float (Renderer w)] -> Renderer w
alignVertically hints = do
	(width, height) <- getCanvasSize
	fmap vertCat (mapM (\ (h, r) ->
	                        fmap (resize width h)
	                             (withReaderT (\ rc -> rc {rcSize = (width, h)}) r))
	                    (divideMetric hints height))

-- | Align elements horizontally.
alignHorizontally :: [DivisionHint Int Float (Renderer w)] -> Renderer w
alignHorizontally hints = do
	(width, height) <- getCanvasSize
	fmap horizCat (mapM (\ (w, r) ->
	                         fmap (resize w height)
	                              (withReaderT (\ rc -> rc {rcSize = (w, height)}) r))
	                    (divideMetric hints width))

-- | Generate a color using an index in [0; 255]
rawColor :: Word8 -> Color
rawColor n
	| n < 16 = ISOColor n
	| otherwise = Color240 n

-- | Make an attribute using a foreground and background color.
mkAttr :: Word8 -> Word8 -> Attr
mkAttr fg bg =
	Attr Default (SetTo (rawColor fg)) (SetTo (rawColor bg))

-- | Types which have a visual representation shall implement this type class.
class Visual a where
	mkRenderer :: Renderer a

instance Visual () where
	mkRenderer = emptyRenderer

instance Visual [Char] where
	mkRenderer = getRenderState >>= drawString mempty

instance Visual T.Text where
	mkRenderer = getRenderState >>= drawText mempty

-- | An alternative for "withRenderer".
withVisual :: (Visual t) => Lens' s t -> Renderer s
withVisual l = withRenderer l mkRenderer
