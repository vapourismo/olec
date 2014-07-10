{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Color (
	-- * Bindings
	bindPair,
	bindColor,
	setRenderPair,

	-- * Colors
	ColorPairID,
	colorBlack,
	colorRed,
	colorGreen,
	colorYellow,
	colorBlue,
	colorMagenta,
	colorCyan,
	colorWhite,
	colorGrey
) where

import Foreign.C

-- | Associate a color pair with a foreground and background.
foreign import ccall unsafe "terminal_bind_pair"
	bindPair :: CInt   -- ^ Pair ID
	         -> CShort -- ^ Foreground
	         -> CShort -- ^ Background
	         -> IO ()

-- | Rebind a color.
--   Most terminals don't support true color and will approximate the color.
foreign import ccall unsafe "terminal_bind_color"
	bindColor :: CShort -- ^ Color ID
	          -> CShort -- ^ Red (0-1000)
	          -> CShort -- ^ Green (0-1000)
	          -> CShort -- ^ Blue (0-1000)
	          -> IO ()

-- | Sets the color pair to be rendered.
foreign import ccall unsafe "terminal_attr_color"
	setRenderPair :: CInt -> IO ()

-- | Color Pair ID
type ColorPairID = CInt

-- | Color ID
type ColorID = CShort

colorBlack, colorRed, colorGreen, colorYellow, colorBlue :: ColorID
colorMagenta, colorCyan, colorWhite, colorGrey :: ColorID

colorBlack   = 0
colorRed     = 1
colorGreen   = 2
colorYellow  = 3
colorBlue    = 4
colorMagenta = 5
colorCyan    = 6
colorWhite   = 7
colorGrey    = 8
