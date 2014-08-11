import Olec.Terminal
import Olec.Input
import Olec.Layout.Pane

main = withTerm $ do
	src <- processInput

	root <- rootPane

	paneMoveCursor root (0, 0)
	paneDrawString root "Hello World"
	termRender

	readInputEvent src
