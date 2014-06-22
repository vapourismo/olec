import Olec.Terminal
import Olec.Terminal.Window
import Olec.Terminal.Layout

main = withTerminal $ do
	scr <- defaultWindow
	let (win1, win2) = split (RelVSplit (-0.25)) scr

	fillWindow win1 '1'
	fillWindow win2 '2'

	render
	getChar
