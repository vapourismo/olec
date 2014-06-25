import Olec.Terminal
import Olec.Terminal.Input
import qualified Olec.Editor as E
import qualified Olec.Editor.Line as L

import Data.Char

import Control.Monad.State

data AppState = AppState { editor :: E.Editor,
                           input :: InputQueue }

type AppStateT = StateT AppState IO

renderEditor :: AppStateT ()
renderEditor = do
	e <- gets editor

	liftIO $ do
		clearTerm

		(scrW, scrH) <- termSize

		let lines = take scrH (map (take scrW . L.visual 4) $ E.rows e)
		let drawLine ln cnt = do
			moveCursor 0 ln
			drawString cnt
			return (ln + 1)

		foldM_ drawLine 0 lines
		render

readInput :: AppStateT InputEvent
readInput = gets input >>= liftIO . readKeyEvent

transfromEditor :: (E.Editor -> E.Editor) -> AppStateT ()
transfromEditor f = do
	s <- get
	put $ s {editor = f (editor s)}

app :: AppStateT ()
app = do
	renderEditor
	input <- readInput
	case input of
		KeyPress ModControl (KeyChar 'q') ->
			return ()
		KeyPress ModNone (KeyChar c) | isPrint c -> do
			transfromEditor (E.appendLine 0 [c])
			app
		_ -> app

main = withTerm $ do
	render
	inQueue <- processInput
	evalStateT app (AppState (E.makeEditor "") inQueue)
