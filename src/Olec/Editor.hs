module Olec.Editor where

import qualified Olec.Editor.Row as R
import qualified Olec.Editor.Cursor as C

data Editor = Editor { rows    :: [R.Row]
                     , numRows :: Int
                     , cursors :: [C.Cursor] }
	deriving Show

replaceRow :: Int -> String -> Editor -> Editor
replaceRow row txt e@(Editor rows numRows cursors)
	| row < numRows =
		Editor (take row rows ++ (R.purge txt : drop (row + 1) rows)) numRows cursors
	| otherwise = e

insert :: Int -> Int -> String -> Editor -> Editor
insert row col txt e
	| row < numRows e = replaceRow row (R.insert (rows e !! row) col txt) e
	| otherwise = e
