module Olec.Render (
	-- * Base types
	Size,
	Position,

	-- * Render types
	RenderM,
	Renderer,
	renderPicture,
	renderImage,
	canvasSize,

	-- * Cursor functions
	putCursor,
	getCursor,
	hideCursor,

	-- * Drawing
	drawText,

	-- * Layouts
	DivisionHint (..),
	alignVertically
) where

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Text as T

import Graphics.Vty hiding (Event, hideCursor)

import Olec.Util

type Size = (Int, Int)

type Position = (Int, Int)

-- | Render environment
type RenderM = ReaderT Size (State Cursor)

-- | Image renderer
type Renderer = RenderM Image

-- | Run the renderer in a canvas with the given size.
renderPicture :: Renderer -> Size -> Picture
renderPicture r s =
	Picture cur [img] ClearBackground
	where (img, cur) = runState (runReaderT r s) NoCursor

-- | Run the renderer but retrieve the image only.
renderImage :: Renderer -> Size -> Image
renderImage r s@(w, h) = resize w h (evalState (runReaderT r s) NoCursor)

-- | Move the cursor to a certain location.
putCursor :: Position -> RenderM ()
putCursor = put . uncurry Cursor

-- | Get the current cursor position.
getCursor :: RenderM Cursor
getCursor = get

-- | Hide the cursor.
hideCursor :: RenderM ()
hideCursor = put NoCursor

-- | Get the gurrent canvas size.
canvasSize :: RenderM Size
canvasSize = ask

-- | Produce an image containing the given text.
drawText :: Attr -> T.Text -> Renderer
drawText attr val = pure (text' attr val)

-- | Align elements vertically.
alignVertically :: [DivisionHint Int Double Renderer] -> Renderer
alignVertically hints =
	flip fmap canvasSize $ \ (width, height) ->
		vertCat (map (\ (h, r) -> renderImage r (width, h))
		             (divideMetric hints height))
