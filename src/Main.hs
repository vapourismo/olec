import Olec.Terminal
import Olec.Input

main = withTerm $ do
	src <- processInput
	readInputEvent src
