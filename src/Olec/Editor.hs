module Olec.Editor (
	-- * Construction
	Editor,
	makeEditor,

	-- * Modification
	transformLine,
	insert,
	appendLine,
	prependLine,
	splitLine,
	splitLine',

	-- * Transforma
	EditorT,
	transformLineT,
	insertT,
	appendLineT,
	prependLineT,
	splitLineT,
	splitLineT'
) where

import qualified Olec.Editor.Line as L

import Control.Monad.State

data Editor = Editor { rows :: [L.Line],
                       numRows :: Int }
	deriving Show

-- | Construct an editor.
makeEditor :: String -> Editor
makeEditor txt = Editor rs (length rs) where
	rs = map L.makeLine (lines txt)

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
appendLine :: Int -> String -> Editor -> Editor
appendLine l txt = transformLine l (L.append txt)

-- | Prepend text to a line.
prependLine :: Int -> String -> Editor -> Editor
prependLine l txt = transformLine l (L.prepend txt)

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

-- | Editor Transformer
type EditorT = State Editor

transformLineT :: Int -> (L.Line -> L.Line) -> EditorT ()
transformLineT l f = modify (transformLine l f)

insertT :: Int -> Int -> String -> EditorT ()
insertT l c txt = modify (insert l c txt)

appendLineT :: Int -> String -> EditorT ()
appendLineT l txt = modify (appendLine l txt)

prependLineT :: Int -> String -> EditorT ()
prependLineT l txt = modify (prependLine l txt)

splitLineT :: Int -> Int -> EditorT ()
splitLineT l c = modify (splitLine l c)

splitLineT' :: Int -> Int -> EditorT ()
splitLineT' l c = modify (splitLine' l c)
