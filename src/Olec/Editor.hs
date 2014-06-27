module Olec.Editor (
	-- * Construction
	Editor,
	rows,
	numRows,
	makeEditor,

	-- * Modification
	transformLine,
	insert,
	append,
	prepend,
	splitLine,
	splitLine'
) where

import qualified Olec.Editor.Line as L

data Editor = Editor { rows :: [L.Line],
                       numRows :: Int }
	deriving Show

-- | Construct an editor.
makeEditor :: String -> Editor
makeEditor txt = Editor rs (length rs) where
	rs = map L.makeLine $ case lines txt of
		[] -> [""]
		xs -> xs

-- | Transform a line using a function.
transformLine :: Int -> (L.Line -> L.Line) -> Editor -> Editor
transformLine l f e
	| l < numRows e = e {rows = newRows}
	| otherwise = e
	where
		(h, line : t) = splitAt l (rows e)
		newRows = h ++ f line : t

-- | Insert text somewhere.
insert :: Int -> Int -> String -> Editor -> Editor
insert l c txt = transformLine l (L.insert c txt)

-- | Append text to a line.
append :: Int -> String -> Editor -> Editor
append l txt = transformLine l (L.append txt)

-- | Prepend text to a line.
prepend :: Int -> String -> Editor -> Editor
prepend l txt = transformLine l (L.prepend txt)

-- | Split a line.
splitLine :: Int -> Int -> Editor -> Editor
splitLine l c e
	| l < numRows e = e {rows = newRows}
	| otherwise = e
	where
		newRows = h ++ x : y : t
		(h, line : t) = splitAt l (rows e)
		(x, y) = L.split c line

-- | Same as 'splitLine' but keeps the indentation level.
splitLine' :: Int -> Int -> Editor -> Editor
splitLine' l c e
	| l < numRows e = e {rows = newRows}
	| otherwise = e
	where
		newRows = h ++ x : y : t
		(h, line : t) = splitAt l (rows e)
		(x, y) = L.split' c line
