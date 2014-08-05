import Olec.Terminal
import Olec.Input
import Olec.Layout.Pane

main = withTerm $ do
	src <- processInput
	readInputEvent src
