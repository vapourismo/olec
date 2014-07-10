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
		setStyle p
		forM_ [0 .. (h - 1)] (renderLine $ replicate w c)

statusCom fileName = Com fileName render pure where
	render msg = do
		moveCursor (0, 0)
		setStyle 2

		drawChar ' '
		drawString msg

		setStyleN (-1) 2

main = withTerm $ do
	src <- processInput

	bindPair 1 colorRed colorGreen
	bindPair 2 colorCyan colorMagenta

	scom <- liftUpdate $ do
		let ca = bgCom 'A' 1
		let cb = statusCom "hello.txt"
		mkSplitCom (AbsHSplit (-1)) ca cb

	liftUpdate (drawCom scom)
	gMoveCursor 0 0
	render

	readInputEvent src
