{-# LANGUAGE ExistentialQuantification #-}

import Olec.Terminal
import Olec.Input
import Olec.Window

import Control.Applicative
import Control.Monad


data Com s = forall a. Com s (s -> Update a) (s -> Update s)


drawCom :: Com s -> Update ()
drawCom (Com s r _) = () <$ r s

updateCom :: Com s -> Update (Com s)
updateCom (Com s r u) = (\s' -> Com s' r u) <$> u s


bgCom c = Com c render pure where
 	renderLine line y = moveCursor (0, y) >> drawString line
	render c = do
		(w, h) <- winSize
		forM_ [0 .. (h - 1)] (renderLine $ replicate w c)

splitCom info a b = do
	(wa, wb) <- splitWindow info
	return $ Com (info, a, b, wa, wb) render update where
		update (info, a, b, _, _) = do
			(wa, wb) <- splitWindow info
			a' <- withWindow wa (updateCom a)
			b' <- withWindow wb (updateCom b)
			return (info, a', b', wa, wb)

		render (info, a, b, wa, wb) = do
			withWindow wa (drawCom a)
			withWindow wb (drawCom b)


main = withTerm $ do
	src <- processInput

	scom <- liftUpdate $ do
		let ca = bgCom 'A'
		let cb = bgCom 'B'
		splitCom (RelVSplit 0.5) ca cb

	render
	readInputEvent src
