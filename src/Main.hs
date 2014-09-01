module Main (main) where

import Olec.Terminal.Input
import Olec.Terminal.Actions


main :: IO ()
main = withTerminal $ do
	input <- processInput

	runAction' $ do
		moveCursor (10, 10)
		drawString "Hello World"
	refresh

	readInputEvent input
	return ()
