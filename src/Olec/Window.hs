module Olec.Window (
	-- * Window
	Window,

	-- * Update
	Update,
	liftUpdate,

	-- * Cursor
	moveCursor,

	-- * Drawing
	drawString,
	drawByteString,
	drawChar,
	renderPair,

	-- * Properties
	winSize,

	-- * Windows
	subWindow,
	withWindow,

	-- * Splitting
	SplitInfo (..),
	splitWindow
) where

import Olec.Terminal
import Olec.Color

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B


-- | A potato.
data Window
	= Window Int Int Int Int
	deriving (Ord, Eq, Show)

-- | Position the cursor relative to the Window's origin.
wMoveCursor :: Position -> Window -> IO ()
wMoveCursor (x, y) (Window wx wy _ _) =
	gMoveCursor (wx + x) (wy + y)

-- | Draw a String within a Window.
wDrawString :: String -> Window -> IO ()
wDrawString str (Window winX winY winW winH) = do
	(x, y) <- gCursor
	when (y >= winY && y < winY + winH && x < winX + winW && x + length str >= winX) $ do
		-- Calculate bounds and x-position
		let (cutFront, cutBack, curX) = fitEntity x winX winW (length str)
		let result = drop cutFront (take (length str - cutBack) str)

		-- When there is something to be drawn
		when (length result > 0) $ do
			gMoveCursor curX y
			gDrawString result

-- | Draw a ByteString within a Window.
wDrawByteString :: B.ByteString -> Window -> IO ()
wDrawByteString str (Window winX winY winW winH) = do
	(x, y) <- gCursor
	when (y >= winY && y < winY + winH && x < winX + winW && x + B.length str >= winX) $ do
		-- Calculate bounds and x-position
		let (cutFront, cutBack, curX) = fitEntity x winX winW (B.length str)
		let result = B.drop cutFront (B.take (B.length str - cutBack) str)

		-- When there is something to be drawn
		when (B.length result > 0) $ do
			gMoveCursor curX y
			gDrawByteString result

-- | Draw a character within the Window.
wDrawChar :: Char -> Window -> IO ()
wDrawChar c win =
	fmap (wEncloses win) gCursor >>= flip when (gDrawChar c)

-- | Fit an entity inside a Window.
fitEntity :: Int -- ^ X-positon
           -> Int -- ^ Window X-origin
           -> Int -- ^ Window width
           -> Int -- ^ Segment length
           -> (-- | Cut front
               Int,
               -- | Cut back
               Int,
               -- | X-position
               Int)
fitEntity x winX winW len =
	(cutFront,
	 cutBack,
	 max winX (min (winX + winW) x)) where
		cutFront = if x < winX then winX - x else 0
		cutBack = if x + len > winX + winW then (x + len) - (winX + winW) else 0

-- | Does the given Position lay within the given Window?
wEncloses :: Window -> Position -> Bool
wEncloses (Window x y w h) (cx, cy) =
	x <= cx && cx < x + w
	&& y <= cy && cy < y + h


-- | Update Capsule
newtype Update a
	= Update { runUpdate :: Window -> IO a }

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

-- | Split a window
splitWindow :: SplitInfo -> Update (Window, Window)
splitWindow info = Update (return . split info)

-- | Get the target Window dimensions.
winSize :: Update Size
winSize = Update $ \(Window _ _ w h) -> return (w, h)

-- | Active a color pair.
renderPair :: ColorPairID -> Update ()
renderPair = Update . const . setRenderPair

-- | Create a new Window.
subWindow :: Position -> Size -> Update Window
subWindow (x, y) (w, h) = Update $ \(Window px py pw ph) -> return $
	if x < pw && y < ph
		then let
			x' = max 0 x
			y' = max 0 y
			w' = min (pw - x') (max 0 w) -- Make the new width fit within the parent window
			h' = min (ph - y') (max 0 h) -- Same for height
			in Window (px + x') (py + y') w' h'
		else Window (px + pw) (py + ph) 0 0 -- Out-of-bounds request are truncated to a nil-Window

-- | Update another Window.
withWindow :: Window -> Update a -> Update a
withWindow w u = Update $ \_ -> runUpdate u w


-- | Apply update to the root window.
liftUpdate :: Update a -> IO a
liftUpdate u = defaultWindow >>= runUpdate u

-- | Fetch the default Window (root).
defaultWindow :: IO Window
defaultWindow = do
	(w, h) <- termSize
	return (Window 0 0 w h)


-- | Split Information
data SplitInfo
	= AbsVSplit Int    -- ^ Absolute Vertical Split
	| AbsHSplit Int    -- ^ Absolute Horizontal Split
	| RelVSplit Float  -- ^ Relative Veritcal Split
	| RelHSplit Float  -- ^ Relative Horizontal Split

-- | Normalize the seperator
normSep sep com
	| sep < 0 = max 0 (min com (com + sep))
	| otherwise = max 0 (min com sep)

-- | Generate an absolute seperator value from a relative seperator
relSep sep com = toInt (sep * toFloat com) where
	toFloat = fromInteger . toInteger
	toInt = floor

-- | Generate two sub windows based on the layout information.
split :: SplitInfo -> Window -> (Window, Window)

-- Absolute Vertical Split
split (AbsVSplit sep') (Window x y w h) = (left, right) where
	sep = normSep sep' w
	left = Window x y sep h
	right = Window (x + sep) y (w - sep) h

-- Absolute Horizontal Split
split (AbsHSplit sep') (Window x y w h) = (top, bottom) where
	sep = normSep sep' h
	top = Window x y w sep
	bottom = Window x (y + sep) w (h - sep)

-- Relative Vertical Split
split (RelVSplit sep') (Window x y w h) = (left, right) where
	sep = normSep (relSep sep' w) w
	left = Window x y sep h
	right = Window (x + sep) y (w - sep) h

-- Relative Horizontal Split
split (RelHSplit sep') (Window x y w h) = (top, bottom) where
	sep = normSep (relSep sep' h) h
	top = Window x y w sep
	bottom = Window x (y + sep) w (h - sep)
