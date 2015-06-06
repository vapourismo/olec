module Olec.Visual.Types (
	-- * General
	Size,
	Position,
	Color (..),
	Style (..)
) where

import Numeric

import Data.Word
import Data.Char
import Data.String

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

instance IsString Color where
	fromString "black"   = Color 0   0   0
	fromString "red"     = Color 255 0   0
	fromString "green"   = Color 0   255 0
	fromString "yellow"  = Color 255 255 0
	fromString "blue"    = Color 0   0   255
	fromString "magenta" = Color 255 0   255
	fromString "cyan"    = Color 0   255 255
	fromString "white"   = Color 255 255 255
	fromString "gray"    = Color 128 128 128

	fromString ('#' : color)
		| all isHexDigit color && length color == 6 =
			let
				hex '1' = 1
				hex '2' = 2
				hex '3' = 3
				hex '4' = 4
				hex '5' = 5
				hex '6' = 6
				hex '7' = 7
				hex '8' = 8
				hex '9' = 9
				hex 'a' = 10
				hex 'b' = 11
				hex 'c' = 12
				hex 'd' = 13
				hex 'e' = 14
				hex 'f' = 15
				hex _   = 0

				[r1, r2, g1, g2, b1, b2] = map (hex . toLower) color

			in Color (r1 * 16 + r2) (g1 * 16 + g2) (b1 * 16 + b2)

	fromString color =
		error ("Invalid color string '" ++ color ++ "'")

-- | Foreground and background color
data Style = Style {
	styleForeground :: Color,
	styleBackground :: Color
} deriving (Show, Eq, Ord)
