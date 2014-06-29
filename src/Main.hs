import Olec.Terminal
import Olec.Terminal.Input

main = withTerm $ do
	src <- processInput
	render

	let loop = do
		ev <- readInputEvent src
		case ev of
			KeyPress ModControl (KeyChar 'q') -> return ()
			event -> do
				drawString (show event)
				drawString "; "
				render
				loop

	loop
