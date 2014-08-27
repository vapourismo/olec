{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Foreign.Ptr
import Olec.Terminal

fillWindow :: Ptr Window -> Char -> IO ()
fillWindow win chr = do
	maxX <- windowMaxX win
	maxY <- windowMaxY win

	let line = replicate (fromIntegral maxX + 1) chr

	forM_ [0 .. maxY] $ \y -> do
		moveCursor win y 0
		drawString win line

	refreshWindow win
	return ()

main :: IO ()
main = withTerm $ do
	win <- newWindow (0, 0) (0, 0)

	x <- windowMaxX win
	y <- windowMaxY win

	fillWindow win '1'
	getChar

	resizeWindow win (10, 10)
	fillWindow win '2'
	getChar

	moveWindow win (10, 10)
	fillWindow win '3'

	-- Wait for input
	getChar
	return ()
