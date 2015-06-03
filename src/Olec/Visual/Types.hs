module Olec.Visual.Types (
	-- * General
	Size,
	Position,
	Color (..),
	Style (..)
) where

import Numeric
import Data.Word

type Size = (Int, Int)

type Position = (Int, Int)

-- | RGB Color
data Color = Color Word8 Word8 Word8
	deriving (Eq, Ord)

instance Show Color where
	show (Color r g b) =
		"#" ++ extendTo2 (showHex r [])
		    ++ extendTo2 (showHex g [])
		    ++ extendTo2 (showHex b [])
		where
			extendTo2 [x] = '0' : [x]
			extendTo2 xs = xs

-- | Foreground and background color
data Style = Style {
	styleForeground :: Color,
	styleBackground :: Color
} deriving (Show, Eq, Ord)
