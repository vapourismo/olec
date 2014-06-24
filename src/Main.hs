import Olec.Terminal
import Olec.Terminal.Input

main = withTerm $ do
	src <- processInput
	render
	readKeyStroke src
