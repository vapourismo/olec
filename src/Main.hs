import Olec.Terminal
import Olec.Terminal.Window
import Olec.Terminal.Input
import Olec.Terminal.Layout
import Olec.Provider

noUpdate _ s = s

providerA = Provider 'A' noUpdate wFill
providerB = Provider 'B' noUpdate wFill

provider = splitProvider (RelVSplit 0.5) providerA providerB

main = withTerm $ do
	src <- processInput

	win <- defaultWindow
	renderProvider (updateProvider provider win) win

	readInputEvent src
