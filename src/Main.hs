{-# LANGUAGE ExistentialQuantification #-}

import Olec.Terminal
import Olec.Terminal.Window
import Olec.Terminal.Layout
import Olec.Terminal.Input

import Control.Monad.State

data AppState = forall a. AppState { statusbar :: Window
                                   , viewport  :: Window
                                   , content   :: Window -> IO a }

type AppStateT = StateT AppState IO

updateLayout :: AppStateT ()
updateLayout = do
	(wViewport, wStatusbar) <- liftIO (fmap (split $ AbsHSplit (-1)) defaultWindow)
	modify (\app -> app {statusbar = wStatusbar, viewport = wViewport})

renderViewport :: AppStateT ()
renderViewport = do
	AppState _ vp cnt <- get
	liftIO (cnt vp >> render)

initAppState = AppState nullWindow nullWindow (\w -> wFill w '#')

main = withTerm $ do
	src <- processInput
	flip execStateT initAppState $ do
		updateLayout
		renderViewport

	readInputEvent src
