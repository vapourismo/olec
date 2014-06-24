module Olec.Editor.Cursor where

data Cursor = InsertionCursor { row :: Int, column :: Int }
            | SelectionCursor { row :: Int, column :: Int, width :: Int }
	deriving Show
