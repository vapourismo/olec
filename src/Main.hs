{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import Data.IORef

import Graphics.UI.Gtk

import Foreign.C

import System.Posix.Types
import System.Posix.Terminal

import Olec.Terminal

isModifierKey :: KeyVal -> Bool
isModifierKey key =
    (0xffe1 <= key && key <= 0xffee) ||
    (0xfe01 <= key && key <= 0xfe0f) ||
    (0xfe11 <= key && key <= 0xfe13) ||
    key == 0xff7e

data Event
    = Resize CLong CLong
    | KeyPress [Modifier] KeyVal
    deriving (Show)

main :: IO ()
main = do
    initGUI
    eventChan <- newChan

    -- Main window
    win <- windowNew
    set win [windowTitle := ("Olec Text Editor" :: String)]
    on win objectDestroy mainQuit

    on win keyPressEvent $ True <$ do
        eval <- eventKeyVal
        emod <- eventModifier

        unless (isModifierKey eval) . lift $ do
            writeChan eventChan (KeyPress emod eval)

    -- Box
    box <- vBoxNew False 0
    containerAdd win box

    -- Terminal
    (Fd ptm, _) <- openPseudoTerminal
    term <- newTerminal ptm
    boxPackStart box term PackGrow 0

    dimRef <- newIORef (0, 0)
    on term sizeAllocate $ \ _ -> do
            tup <- terminalSize term
            tup' <- readIORef dimRef
            when (tup /= tup') $ do
                writeIORef dimRef tup
                writeChan eventChan (uncurry Resize tup)

    -- Dispatch events
    forkIO (forever (readChan eventChan >>= print))

    -- Run
    widgetShowAll win
    mainGUI
