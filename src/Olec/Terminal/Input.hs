module Olec.Terminal.Input (KeyStroke (..), parseInput, readSome) where

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
               | KeyUndefined Char
	deriving (Show, Eq)

-- Is c control modified?
isCtrlMod c = 1 <= ord c && ord c <= 26

-- Fix the control modifier offset
fixCtrlOffset c = chr (ord c + 96)

-- | Parse an input sequence.
parseInput :: [Char] -> [KeyStroke]
parseInput cs = parseInput' cs []

-- Special sequences
parseInput' "\r"       t = KeySpecial KeyNoMod KeyReturn : parseInput' t []
parseInput' "\n"       t = KeySpecial KeyCtrl  KeyReturn : parseInput' t []
parseInput' "\ESC\n"   t = KeySpecial KeyAlt   KeyReturn : parseInput' t []
parseInput' "\ESC\r"   t = KeySpecial KeyAlt   KeyReturn : parseInput' t []
parseInput' "\ESC"     t = KeySpecial KeyNoMod KeyEscape : parseInput' t []
parseInput' "\ESC\ESC" t = KeySpecial KeyAlt   KeyEscape : parseInput' t []

-- Ordinary characters
parseInput' (x : [])   t
	| isPrint x   = KeyPrint KeyNoMod x : parseInput' t []
	| isCtrlMod x = KeyPrint KeyCtrl (fixCtrlOffset x) : parseInput' t []

-- Alt + Ordinary character
parseInput' ('\ESC' : x : []) t
	| isPrint x   = KeyPrint KeyAlt x : parseInput' t []
	| isCtrlMod x = KeyPrint KeyCtrlAlt (fixCtrlOffset x) : parseInput' t []

-- Anything else
parseInput' [] []       = []
parseInput' [] (t : ts) = KeyUndefined t : parseInput' ts []
parseInput' xs t        = parseInput' (init xs) (last xs : t)

-- | Read some characters
readSome :: IO [Char]
readSome = fmap B.unpack (B.hGetSome stdin 64)
