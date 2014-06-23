import Olec.Terminal
import Olec.Terminal.Input

loop y my = do
	input <- readKey

	moveCursor 0 y
	drawString (show input)
	clearToEOL
	render

	case input of
		KeyPrint _ 'q' -> return ()
		_              -> loop (mod (y + 1) my) my

main = withTerm $ do
	(_, h) <- termDimension
	render
	loop 0 (h - 1)
