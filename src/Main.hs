import Olec.Terminal
import Olec.Input
import Olec.Window
import Olec.Component

import Control.Applicative
import Control.Monad

bgCom c = Com c render pure where
 	renderLine line y = moveCursor (0, y) >> drawString line
	render c = do
		(w, h) <- winSize
		forM_ [0 .. (h - 1)] (renderLine $ replicate w c)

main = withTerm $ do
	src <- processInput

	scom <- liftUpdate $ do
		let ca = bgCom 'A'
		let cb = bgCom 'B'
		mkSplitCom (RelVSplit 0.5) ca cb

	liftUpdate (drawCom scom)
	render

	readInputEvent src
