import Olec.Terminal
import Olec.Terminal.Window
import Olec.Terminal.Input

data Provider s a = Provider { state :: s
                             , updateHook :: Window -> s -> IO s
                             , renderHook :: Window -> s -> IO a }

updateProvider (Provider s u r) w = u w s >>= \s' -> return (Provider s' u r)
renderProvider (Provider s _ r) w = r w s >> render

fillProvider = Provider '#' noUpdate wFill where
	noUpdate _ s = return s

main = withTerm $ do
	src <- processInput

	win <- defaultWindow
	renderProvider fillProvider win

	readInputEvent src
