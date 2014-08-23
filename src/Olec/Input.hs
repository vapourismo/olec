module Olec.Input (
	-- * Types
	InputEvent (..),
	Modifier (..),
	Key (..),
	InputQueue,

	-- * Input Processing
	processInput,
	readInputEvent
) where

import System.IO
import Data.Char
import Control.Monad
import Control.Concurrent


data Modifier
	= ModControl
	| ModAlt
	| ModShift
	| ModControlAlt
	| ModControlAltShift
	| ModControlShift
	| ModAltShift
	| ModNone
	deriving (Show, Eq)

data Key
	= KeyChar Char
	| KeyReturn
	| KeyEscape
	| KeyTab
	| KeyBackspace
	| KeyArrowLeft
	| KeyArrowRight
	| KeyArrowUp
	| KeyArrowDown
	deriving (Show, Eq)

data InputEvent
	= KeyPress Modifier Key
    | Undefined Char
    | Escaped [String] Char
	deriving (Show, Eq)

type InputQueue = Chan InputEvent


-- | Is c control modified?
isCtrlMod :: Char -> Bool
isCtrlMod c = 1 <= ord c && ord c <= 26

-- | Fix the control modifier offset
fixCtrlOffset :: Char -> Char
fixCtrlOffset c = chr (ord c + 96)

-- | Translate parameters passed to escape sequences with a movement command (e.g. A, B, C, D, etc.)
translateArrowMod :: [String] -> Modifier
translateArrowMod [_, "2"] = ModShift
translateArrowMod [_, "3"] = ModAlt
translateArrowMod [_, "4"] = ModAltShift
translateArrowMod [_, "5"] = ModControl
translateArrowMod [_, "6"] = ModControlShift
translateArrowMod [_, "7"] = ModControlAlt
translateArrowMod [_, "8"] = ModControlAltShift
translateArrowMod _ = ModNone

-- | Parse an escape sequence.
parseEscSeq :: Char -> [String] -> InputEvent
parseEscSeq 'A' params = KeyPress (translateArrowMod params) KeyArrowUp
parseEscSeq 'B' params = KeyPress (translateArrowMod params) KeyArrowDown
parseEscSeq 'C' params = KeyPress (translateArrowMod params) KeyArrowRight
parseEscSeq 'D' params = KeyPress (translateArrowMod params) KeyArrowLeft
parseEscSeq cmd params = Escaped params cmd

-- | Parse an input sequence.
parseInput :: [Char] -> [InputEvent]
parseInput cs = parseInput' cs [] where
	-- Escape sequence
	parseInput' ('\ESC' : '[' : xs) t =
		parseEscSeq r (splitParams h) : parseInput (rs ++ t) where
			-- Break the escape sequence
			-- 'h' contains the parameters seperated by ';'
			-- 'r' is the command character
			(h, r : rs) = break (`notElem` "0123456789;") xs

			-- Split a string using the ';' character
			splitParams [] = []
			splitParams ys = case break (== ';') ys of
				(a, ';' : as) -> a : splitParams as
				(a, _) -> [a]

	-- Esc/Alt modified
	parseInput' ('\ESC' : xs) t =
		case parseInput (xs ++ t) of
			-- Normal key presses can be modified by Alt
			KeyPress mod key : rs -> KeyPress mod' key : rs where
				mod' = case mod of
					ModControl       | key == KeyReturn -> ModAlt
					ModControl      -> ModControlAlt
					ModControlShift -> ModControlAltShift
					ModShift        -> ModAltShift
					ModNone         -> ModAlt
					m               -> m

			-- Keep the rest the way it is
			rs -> KeyPress ModNone KeyEscape : rs

	-- Special sequences
	parseInput' "\n"   t = KeyPress ModControl KeyReturn : parseInput t
	parseInput' "\r"   t = KeyPress ModNone KeyReturn    : parseInput t
	parseInput' "\t"   t = KeyPress ModNone KeyTab       : parseInput t
	parseInput' "\DEL" t = KeyPress ModNone KeyBackspace : parseInput t

	-- Ordinary characters
	parseInput' (x : []) t
		| isPrint x   = KeyPress ModNone (KeyChar x) : parseInput t
		| isCtrlMod x = KeyPress ModControl (KeyChar $ fixCtrlOffset x) : parseInput t

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
		keys <- readSome
		forM_ (parseInput keys) (writeChan q)
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
