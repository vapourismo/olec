module Olec.Terminal.Input (
	-- * Key Types
	InputEvent (..),
	Modifier (..),
	Key (..),

	-- * Input Processing
	InputQueue,
	processInput,
	readInputEvent
) where

import System.IO
import Data.Char
import Control.Monad
import Control.Concurrent

data Modifier
	= ModControl
	| ModControlAlt
	| ModAlt
	| ModNone
	deriving (Show, Eq)

data Key
	= KeyChar Char
	| KeyReturn
	| KeyEscape
	| KeyBackspace
	deriving (Show, Eq)

data InputEvent
	= KeyPress Modifier Key
    | Undefined Char
	deriving (Show, Eq)

type InputQueue = Chan InputEvent

-- Is c control modified?
isCtrlMod c = 1 <= ord c && ord c <= 26

-- Fix the control modifier offset
fixCtrlOffset c = chr (ord c + 96)

-- | Parse an input sequence.
parseInput :: [Char] -> [InputEvent]
parseInput cs = parseInput' cs []

-- Special sequences
parseInput' "\r"       t = KeyPress ModNone    KeyReturn : parseInput' t []
parseInput' "\n"       t = KeyPress ModControl KeyReturn : parseInput' t []
parseInput' "\ESC\n"   t = KeyPress ModAlt     KeyReturn : parseInput' t []
parseInput' "\ESC\r"   t = KeyPress ModAlt     KeyReturn : parseInput' t []
parseInput' "\ESC"     t = KeyPress ModNone    KeyEscape : parseInput' t []
parseInput' "\ESC\ESC" t = KeyPress ModAlt     KeyEscape : parseInput' t []
parseInput' "\DEL"     t = KeyPress ModNone    KeyBackspace : parseInput' t []
parseInput' "\ESC\DEL" t = KeyPress ModAlt     KeyBackspace : parseInput' t []

-- Ordinary characters
parseInput' (x : [])   t
	| isPrint x   = KeyPress ModNone (KeyChar x) : parseInput' t []
	| isCtrlMod x = KeyPress ModControl (KeyChar $ fixCtrlOffset x) : parseInput' t []

-- ModAlt + Ordinary character
parseInput' ('\ESC' : x : []) t
	| isPrint x   = KeyPress ModAlt (KeyChar x) : parseInput' t []
	| isCtrlMod x = KeyPress ModControlAlt (KeyChar $ fixCtrlOffset x) : parseInput' t []

-- Anything else
parseInput' [] []       = []
parseInput' [] (t : ts) = Undefined t : parseInput' ts []
parseInput' xs t        = parseInput' (init xs) (last xs : t)

-- | Read all available characters.
readSome :: IO [Char]
readSome = do
	ready <- hReady stdin
	if ready
		then liftM2 (:) (hGetChar stdin) readSome
		else return []

-- | Process the standard input and push the resulting InputEvents
--   onto the InputQueue.
inputWorker :: InputQueue -> IO ()
inputWorker q = do
	eof <- hIsEOF stdin
	when (not eof) $ do
		keys <- fmap parseInput readSome
		forM_ keys (writeChan q)
		inputWorker q

-- | Start processing InputEvents.
processInput :: IO InputQueue
processInput = do
	q <- newChan
	forkIO (inputWorker q)
	return q

-- | Fetch an InputEvent.
readInputEvent :: InputQueue -> IO InputEvent
readInputEvent = readChan
