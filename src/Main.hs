import Olec.Terminal
import Olec.Terminal.Window

import Control.Lens
import Control.Monad.State

type ViewStateT = StateT ViewState IO

data ViewState = ViewState { _statusbar :: Window
                           , _viewport  :: Window }
	deriving Show

statusbar :: Lens' ViewState Window
statusbar = lens _statusbar (\f x -> f { _statusbar = x })

viewport :: Lens' ViewState Window
viewport = lens _viewport (\f x -> f { _viewport = x })

updateView :: Window -> ViewStateT ()
updateView win = do
	statusbar .= win
	viewport .= win

main = do
	stdscr <- withTerminal defaultWindow
	(a, s) <- runStateT (updateView stdscr) (ViewState nullWindow nullWindow)

	print a
	print s
