{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Olec.Visual.VT100 (VT100 (..)) where

import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Olec.Visual.Types
import Olec.Visual.Image

-- | VT100 Sequence
newtype VT100 = VT100 { toVT100ByteString :: B.ByteString }
	deriving (Monoid)

instance ImageIR VT100 where
	mkSetForeground (Color r g b) =
		VT100 (B.concat ["\ESC[38;2;",
		                 BC.pack (show r), ";",
		                 BC.pack (show g), ";",
		                 BC.pack (show b), "m"])

	mkSetBackground (Color r g b) =
		VT100 (B.concat ["\ESC[48;2;",
		                 BC.pack (show r), ";",
		                 BC.pack (show g), ";",
		                 BC.pack (show b), "m"])

	mkMoveCursor (x, y) =
		VT100 (B.concat ["\ESC[",
		                 BC.pack (show (y + 1)), ";",
		                 BC.pack (show (x + 1)), "H"])

	mkText = VT100 . T.encodeUtf8
