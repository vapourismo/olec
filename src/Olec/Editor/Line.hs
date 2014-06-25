module Olec.Editor.Line (
	-- * Construction
	Line,
	makeLine,

	-- * Modification
	insert,
	append,
	prepend,

	-- * Extraction
	split,
	split',
	raw,
	visual
) where

import Data.Char

data Line = Line { indentLevel :: Int,
                   contents :: String }
	deriving Show

-- | Construct a line.
makeLine :: String -> Line
makeLine txt = Line (length ind) (clean contents) where
	(ind, contents) = break (/= '\t') txt

-- | Insert text into the line.
insert :: Int -> String -> Line -> Line
insert c txt line = makeLine (h ++ txt ++ t) where
	(h, t) = splitAt c (raw line)

-- | Append text to the line.
append :: String -> Line -> Line
append txt line = makeLine (raw line ++ txt)

-- | Prepend text to the line.
prepend :: String -> Line -> Line
prepend txt line = makeLine (txt ++ raw line)

-- | Split a line
split :: Int -> Line -> (Line, Line)
split c line = (makeLine left, makeLine right) where
	(left, right) = splitAt c (raw line)

-- | Same as 'split' but keeps the indentation level.
split' :: Int -> Line -> (Line, Line)
split' c line = (mLeft, mRight) where
	(left, right) = splitAt c (raw line)
	mLeft = makeLine left
	mRight = (makeLine right) {indentLevel = indentLevel mLeft}

-- | Get the raw line contents.
raw :: Line -> String
raw line = replicate (indentLevel line) '\t' ++ contents line

-- | Get the visual representation of the line.
visual :: Int -> Line -> String
visual tabWidth line = replicate (indentLevel line * tabWidth) ' ' ++ contents line

-- | Clean a string.
clean :: String -> String
clean = filter isPrint . map tabs where
	tabs x | elem x "\b\a\v\f\t" = ' '
	       | otherwise = x
