import Olec.Terminal
import Olec.Terminal.Input
import Olec.Terminal.Window

main = withTerm $ do
	src <- processInput
	render
	readKeyEvent src
