import Olec.Editor

import Control.Monad.State

editor = makeEditor "\t\tHello\nWorld"

trans = do
	splitLineT' 0 7
	appendLineT 1 "lovely"

main = print (execState trans editor)
