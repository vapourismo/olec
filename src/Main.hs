import Olec.Terminal
import Olec.Terminal.Window

main = withTerminal $ do
	scr <- defaultWindow
	let win = subWindow scr 0 0 10 10
	fillWindow win '#'
	render
	getChar
