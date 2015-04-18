{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Control.Monad
import Control.Concurrent
import Graphics.UI.Gtk
import System.Posix.Types
import System.Posix.Terminal
import Olec.Terminal
import Olec.Events

main :: IO ()
main = do
    initGUI
    eventChan <- newChan

    -- Main window
    win <- windowNew
    set win [windowTitle := ("Olec Text Editor" :: String)]
    on win objectDestroy mainQuit

    -- Box
    box <- vBoxNew False 0
    containerAdd win box

    -- Terminal
    (Fd ptm, _) <- openPseudoTerminal
    term <- newTerminal ptm
    boxPackStart box term PackGrow 0

    -- Dispatch events
    forwardKeyPressEvents win eventChan
    forwardResizeEvents term eventChan
    forkIO (forever (readChan eventChan >>= print))

    -- Run
    widgetShowAll win
    mainGUI
