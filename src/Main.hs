import Olec.Terminal
import Olec.Terminal.Input
import Data.List

loop y my = do
	keys <- fmap (parseInput) readSome

	let displayKey y k =
		(mod (y + 1) my, moveCursor 0 y >> drawString (show k) >> clearToEOL)

	let (endY, ops) = mapAccumL displayKey y keys

	sequence ops
	render

	let cond k = case k of
		KeyPrint _ 'q' -> True
		_              -> False

	if any cond keys
		then return ()
		else loop endY my

main = withTerm $ do
	(_, h) <- termDimension
	render
	loop 0 (h - 1)

