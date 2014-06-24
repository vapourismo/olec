module Olec.Terminal.Input (
	-- * Key Types
	KeyStroke (..),
	KeyModifier (..),
	SpecialKey (..),

	-- * Input Processing
	KeyQueue,
	processInput,
	readKeyStroke
) where

import System.IO
import Data.Char
import Control.Monad
import Control.Concurrent

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

-- | Read all available characters.
readSome :: IO [Char]
readSome = do
	ready <- hReady stdin
	if ready
		then liftM2 (:) (hGetChar stdin) readSome
		else return []

-- | Source to KeyStrokes from an input worker.
type KeyQueue = Chan KeyStroke

-- | Process the standard input and push the resulting KeyStrokes
--   onto the KeyQueue.
inputWorker :: KeyQueue -> IO ()
inputWorker q = do
	eof <- hIsEOF stdin
	when (not eof) $ do
		keys <- fmap parseInput readSome
		forM_ keys (writeChan q)
		inputWorker q

-- | Start processing input events.
processInput :: IO KeyQueue
processInput = do
	q <- newChan
	forkIO (inputWorker q)
	return q

-- | Fetch a KeyStroke.
readKeyStroke :: KeyQueue -> IO KeyStroke
readKeyStroke = readChan
