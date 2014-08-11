import Olec.Terminal
import Olec.Input
import Olec.Layout.Pane

import Control.Monad


fillPane pane chr = do
	let (w, h) = paneSize pane
	let str = replicate w chr
	forM_ [0 .. h - 1] $ \y -> do
		paneMoveCursor pane (0, y)
		paneDrawString pane str

main = withTerm $ do
	src <- processInput

	root <- rootPane
	let p1 = derivePane (15, 15) (5, 5) root
	let p2 = derivePane (10, 10) (15, 15) p1
	let p3 = derivePane (10, 10) (15, 15) root

	fillPane p3 '3'
	fillPane p1 '1'
	fillPane p2 '2'
	termRender

	readInputEvent src
