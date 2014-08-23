import Olec.Terminal
import Olec.Input
import Olec.Layout.Pane

import Control.Monad


fillPane :: Pane -> Char -> IO ()
fillPane pane chr = do
	let (w, h) = paneSize pane
	let str = replicate w chr
	forM_ [0 .. h - 1] $ \y -> do
		paneMoveCursor pane (0, y)
		paneDrawString pane str

main :: IO ()
main = withTerm $ do
	src <- processInput

	base <- newPane (10, 10) (15, 15)
	p1 <- newPane (15, 15) (5, 5)
	let p2 = derivePane (10, 10) (15, 15) p1

	fillPane base '#'
	fillPane p1 '1'
	fillPane p2 '2'
	termRender

	void (readInputEvent src)
