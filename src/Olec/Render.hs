{-# LANGUAGE ExistentialQuantification #-}

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
	drawText',
	drawString,
	fillChar,

	-- * Layouts
	DivisionHint (..),
	alignVertically,
	alignHorizontally
) where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Graphics.Vty hiding (Event, hideCursor)

import Olec.Util

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
drawText :: Attr -> TL.Text -> Renderer w
drawText attr val = pure (text attr val)

-- | Produce an image containing the given "Text".
drawText' :: Attr -> T.Text -> Renderer w
drawText' attr val = pure (text' attr val)

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
	fmap vertCat (mapM (\ (h, r) -> withReaderT (\ rc -> rc {rcSize = (width, h)}) r)
	                    (divideMetric hints height))

-- | Align elements horizontally.
alignHorizontally :: [DivisionHint Int Float (Renderer w)] -> Renderer w
alignHorizontally hints = do
	(width, height) <- getCanvasSize
	fmap horizCat (mapM (\ (w, r) -> withReaderT (\ rc -> rc {rcSize = (w, height)}) r)
	                    (divideMetric hints width))
