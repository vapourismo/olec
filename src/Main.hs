import Olec.Terminal
import Olec.Input

import Control.Monad

main :: IO ()
main = withTerm $ do
	src <- processInput

	void (readInputEvent src)
