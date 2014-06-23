module Olec.Terminal.Input (KeyStroke (..), readKey) where

import Data.Char
import System.IO (stdin)
import qualified Data.ByteString.Char8 as B

-- | Key Modifer
data KeyModifier = KeyCtrl | KeyCtrlAlt | KeyAlt | KeyNoMod
	deriving (Show, Eq)

-- | Special Keys
data SpecialKey = KeyReturn
                | KeyEscape
	deriving (Show, Eq)

-- | Key Stroke
data KeyStroke = KeyPrint     KeyModifier Char
               | KeySpecial   KeyModifier SpecialKey
               | KeyUndefined [Char]
	deriving (Show, Eq)

-- Is c control modified?
isCtrlMod c = 1 <= ord c && ord c <= 26

-- Fix the control modifier offset
fixCtrlOffset c = chr (ord c + 96)

-- Special sequences
parseInput "\r"       = KeySpecial KeyNoMod KeyReturn
parseInput "\n"       = KeySpecial KeyCtrl  KeyReturn
parseInput "\ESC\n"   = KeySpecial KeyAlt   KeyReturn
parseInput "\ESC\r"   = KeySpecial KeyAlt   KeyReturn
parseInput "\ESC"     = KeySpecial KeyNoMod KeyEscape
parseInput "\ESC\ESC" = KeySpecial KeyAlt   KeyEscape

-- Ordinary characters
parseInput (x : [])
	| isPrint x       = KeyPrint KeyNoMod x
	| isCtrlMod x     = KeyPrint KeyCtrl  (fixCtrlOffset x)

-- Alt + Ordinary character
parseInput ('\ESC' : x : [])
	| isPrint x       = KeyPrint KeyAlt     x
	| isCtrlMod x     = KeyPrint KeyCtrlAlt (fixCtrlOffset x)

-- Recycle bad sequences
-- This is bad! The proper way would be to put all input facilities into a State Monad
-- and cache left-overs.
parseInput (_ : xs)   = parseInput xs

-- Anything else
parseInput xs         = KeyUndefined xs

-- | Read a KeyStroke.
readKey :: IO KeyStroke
readKey = do
	input <- B.hGetSome stdin 10
	return $ if B.length input > 0
		then parseInput (B.unpack input)
		else KeyUndefined []

