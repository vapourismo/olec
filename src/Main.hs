import Olec.Terminal
import Olec.Color
import Olec.Input
import Olec.Window
import Olec.Component

import Control.Applicative
import Control.Monad

bgCom c p = Com c render pure where
 	renderLine line y = moveCursor (0, y) >> drawString line
	render c = do
		(w, h) <- winSize
		renderPair p
		forM_ [0 .. (h - 1)] (renderLine $ replicate w c)

main = withTerm $ do
	src <- processInput

	bindColor 10 1000 0    0
	bindColor 11 0    1000 0

	bindPair 1 colorRed colorGreen
	bindPair 2 colorCyan colorMagenta

	scom <- liftUpdate $ do
		let ca = bgCom 'A' 1
		let cb = bgCom 'B' 2
		mkSplitCom (RelVSplit 0.5) ca cb

	liftUpdate (drawCom scom)
	render

	readInputEvent src
