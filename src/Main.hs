import Olec.Terminal
import Olec.Input
import Olec.Window

r :: Update ()
r = do
	moveCursor (10, 10)
	windowSize >>= drawString . show

main = withTerm $ do
	src <- processInput

	w <- defaultWindow
	runUpdate r w
	render

	readInputEvent src
