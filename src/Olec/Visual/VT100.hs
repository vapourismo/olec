{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Olec.Visual.VT100 (VT100 (..)) where

import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Olec.Visual.Types
import Olec.Visual.Image

-- | VT100 Sequence
newtype VT100 = VT100 { toVT100Sequence :: B.ByteString }
	deriving (Monoid)

instance ImageIR VT100 where
	mkEntity (x, y) (Color fr fg fb) (Color br bg bb) txt = do
		VT100 (B.concat ["\ESC[",
		                 BC.pack (show (y + 1)), ";",
		                 BC.pack (show (x + 1)), "H",
		                 "\ESC[38;2;",
		                 BC.pack (show fr), ";",
		                 BC.pack (show fg), ";",
		                 BC.pack (show fb), "m",
		                 "\ESC[48;2;",
		                 BC.pack (show br), ";",
		                 BC.pack (show bg), ";",
		                 BC.pack (show bb), "m",
		                 T.encodeUtf8 txt])
