import Olec.Terminal
import Olec.Terminal.Window
import Olec.Terminal.Layout

import Control.Applicative

renderA _ win = fillWindow win 'A'
renderB _ win = fillWindow win 'B'

myLayout :: SplitLayout ()
myLayout =
	SplitLayout (RelHSplit 0.5)
		(Renderer nullWindow renderA)
		(Renderer nullWindow renderB)

main = withTerminal $ do
	lay <- updateLayout myLayout <$> defaultWindow
	renderLayout lay ()

	render
	getChar
