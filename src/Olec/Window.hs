module Olec.Window (
	-- * Window
	Window,
	defaultWindow,

	-- * Update
	Update,
	runUpdate,

	-- * Cursor
	moveCursor,

	-- * Drawing
	drawString,
	drawByteString,
	drawChar,

	-- * Properties
	windowSize,
) where

import Olec.Terminal

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B


-- | A potato.
type Window = (Int, Int, Int, Int)

-- | Update Capsule
newtype Update a = Update { runUpdate :: Window -> IO a }


-- Instances for Update
instance Functor Update where
	fmap f (Update g) = Update (fmap f . g)

instance Applicative Update where
	pure = Update . const . pure
	Update f <*> Update g = Update (\w -> f w <*> g w)

instance Monad Update where
	return = pure
	Update f >>= g = Update $ \w ->
		f w >>= \r -> runUpdate (g r) w


-- | Move the cursor.
moveCursor :: Position -> Update ()
moveCursor = Update . wMoveCursor

-- | Draw a String.
drawString :: String -> Update ()
drawString = Update . wDrawString

-- | Draw a ByteString.
drawByteString :: B.ByteString -> Update ()
drawByteString = Update . wDrawByteString

-- | Draw a Char.
drawChar :: Char -> Update ()
drawChar = Update . wDrawChar

-- | Get the target Window dimensions
windowSize :: Update Size
windowSize = Update (return . wDimension)


-- | Fetch the default Window (root).
defaultWindow :: IO Window
defaultWindow = do
	(w, h) <- termSize
	return (0, 0, w, h)

-- | Retrieve the Window's width and height.
wDimension :: Window -> Size
wDimension (_, _, w, h) = (w, h)


-- | Position the cursor relative to the Window's origin.
wMoveCursor :: Position -> Window -> IO ()
wMoveCursor (x, y) (wx, wy, _, _) =
	gMoveCursor (wx + x) (wy + y)


-- | Draw a String within a Window.
wDrawString :: String -> Window -> IO ()
wDrawString str (winX, winY, winW, winH) = do
	(x, y) <- gCursor
	when (y >= winY && y < winY + winH && x < winX + winW && x + length str >= winX) $ do
		let (cutFront, cutBack, curX) = wFitEntity x winX winW (length str)
		let result = drop cutFront (take (length str - cutBack) str)
		when (length result > 0) $ do
			gMoveCursor curX y
			gDrawString result

-- | Draw a ByteString within a Window.
wDrawByteString :: B.ByteString -> Window -> IO ()
wDrawByteString str (winX, winY, winW, winH) = do
	(x, y) <- gCursor
	when (y >= winY && y < winY + winH && x < winX + winW && x + B.length str >= winX) $ do
		let (cutFront, cutBack, curX) = wFitEntity x winX winW (B.length str)
		let result = B.drop cutFront (B.take (B.length str - cutBack) str)
		when (B.length result > 0) $ do
			gMoveCursor curX y
			gDrawByteString result

-- | Draw a character within the Window.
wDrawChar :: Char -> Window -> IO ()
wDrawChar c win =
	fmap (wEncloses win) gCursor >>= flip when (gDrawChar c)

-- -- | Fill a window with a given character.
-- _wFill :: Window -> Char -> IO ()
-- _wFill win c = forM_ [0 .. (h - 1)] (renderLine $ replicate w c) where
-- 	(w, h) = wDimension win
-- 	renderLine line y = wMoveCursor (0, y) win >> gDrawString line


-- | Fit an entity inside a Window.
wFitEntity :: Int -- ^ X-positon
           -> Int -- ^ Window X-origin
           -> Int -- ^ Window width
           -> Int -- ^ Segment length
           -> (-- | Cut front
               Int,
               -- | Cut back
               Int,
               -- | X-position
               Int)
wFitEntity x winX winW len =
	(cutFront, cutBack, max winX (min (winX + winW) x)) where
		cutFront = if x < winX
			then winX - x
			else 0
		cutBack = if x + len > winX + winW
			then (x + len) - (winX + winW)
			else 0

-- | Does the given Position lay within the given Window?
wEncloses :: Window -> Position -> Bool
wEncloses (x, y, w, h) (cx, cy) =
	x <= cx && cx < x + w
	&& y <= cy && cy < y + h


-- -- | Create a Window within another Window.
-- --   The given x- and y-coordinates are relative to the Window's origin.
-- subWindow :: Window -> Position -> Size -> Window
-- subWindow (origX, origY, origW, origH) (x', y') (w', h') = (x, y, w, h) where
-- 	x = max 0 $ min (origX + origW) (max origX x')
-- 	y = max 0 $ min (origY + origH) (max origY y')
-- 	w = max 0 $ min (origW - (x - origX)) w'
-- 	h = max 0 $ min (origH - (y - origY)) h'

-- -- | Create an entirely new window.
-- newWindow :: Position -> Size -> IO Window
-- newWindow pos dim = do
-- 	scr <- defaultWindow
-- 	return (subWindow scr pos dim)
