import Olec.Terminal
import Olec.Terminal.Input

import Control.Concurrent

showKey (KeyPrint mod k) = "KeyPrint " ++ show mod ++ " '" ++ k : "'"
showKey x = show x

loop q y my = do
	k <- readChan q

	moveCursor 0 y

	drawString (showKey k)
	clearToEOL

	render

	case k of
		KeyPrint _ 'q' -> return ()
		_              -> loop q (mod (y + 1) my) my

main = withTerm $ do
	q <- newChan
	forkIO (inputWorker q)

	(_, h) <- termDimension
	render

	loop q 0 (h - 1)
