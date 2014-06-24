module Olec.Editor.Row where

type Row = String

purge :: String -> String
purge = map purgeTabs . filter (not . cond) where
	cond x = x `elem` "\b\a\v\f\n\r"
	purgeTabs '\t' = ' '
	purgeTabs x = x

insert :: Row -> Int -> String -> Row
insert old col txt = take col old ++ purge txt ++ drop col old
