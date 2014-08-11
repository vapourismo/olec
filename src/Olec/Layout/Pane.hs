module Olec.Layout.Pane (
	-- * Pane
	Pane (..),
	rootPane,
	derivePane,
	paneSize,

	-- * Cursor
	paneMoveCursor,

	-- * Drawing
	paneDrawString
) where

import Olec.Terminal
import Control.Applicative
import Control.Monad
--import qualified Data.ByteString as B


-- | A cat.
data Pane = Pane { paneOrigin  :: (Int, Int)
                 , paneStopper :: (Int, Int) }

-- | Root pane.
rootPane :: IO Pane
rootPane = Pane (0, 0) <$> termSize

-- | Create a new pane within an existing one.
derivePane :: (Int, Int) -> (Int, Int) -> Pane -> Pane
derivePane pos bounds p =
	maybe (Pane (0, 0) (0, 0)) make (fitPanes pos bounds p) where
		make (left, top, right, bottom) = Pane (left, top) (right, bottom)

-- | Get the pane's width and height.
paneSize :: Pane -> (Int, Int)
paneSize (Pane (ox, oy) (sx, sy)) = (sx - ox, sy - oy)

-- | Fit an entity within a pane.
fitIntoPane :: Int -> Int -> Pane -> Maybe (Int, Int)
fitIntoPane x w (Pane (ox, _) (sx, _))
	| left < sx && len > 0 = Just (left, len)
	| otherwise = Nothing
	where
		left = max x ox
		right = min sx (x + w)
		len = right - left

-- | Fit a pane within a pane.
fitPanes :: (Int, Int) -> (Int, Int) -> Pane -> Maybe (Int, Int, Int, Int)
fitPanes (x, y) (w, h) (Pane (ox, oy) (sx, sy))
	| left < sx && top < sy && width > 0 && height > 0 = Just (left, top, right, bottom)
	| otherwise = Nothing
	where
		left = max x ox
		right = min sx (x + w)
		top = max y oy
		bottom = min sy (y + h)
		width = right - left
		height = bottom - top

-- | Move the cursor relative to the given pane's origin.
paneMoveCursor :: Pane -> (Int, Int) -> IO ()
paneMoveCursor (Pane (ox, oy) _) (x, y) = termMoveCursor (ox + x, oy + y)

-- | Draw within a given pane.
paneDrawString :: Pane -> String -> IO ()
paneDrawString p@(Pane (_, oy) (_, sy)) str = do
	(cx, cy) <- termGetCursor
	let draw (left, len) = do
		termMoveCursor (left, cy)
		termDrawString $ take len (drop (left - cx) str)
	when (oy <= cy && cy < sy) $ maybe (return ()) draw (fitIntoPane cx (length str) p)
