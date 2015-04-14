{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Control.Concurrent
import Control.Monad.Reader

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Vte.Vte

import Graphics.Vty

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import System.Posix.Types

foreign import ccall "openpty"
    openpty :: Ptr CInt -> Ptr CInt -> Ptr () -> Ptr () -> Ptr () -> IO CInt

createPseudoTerminal :: IO (CInt, CInt)
createPseudoTerminal =
    with 0 $ \ ptmPtr -> with 0 $ \ ptsPtr -> do
        r <- openpty ptmPtr ptsPtr nullPtr nullPtr nullPtr
        when (r /= 0) (error "Could not create pseudo-terminal")

        ptm <- peek ptmPtr
        pts <- peek ptsPtr

        return (ptm, pts)

isModifierKey :: KeyVal -> Bool
isModifierKey key =
    (0xffe1 <= key && key <= 0xffee) ||
    (0xfe01 <= key && key <= 0xfe0f) ||
    (0xfe11 <= key && key <= 0xfe13) ||
    key == 0xff7e

main :: IO ()
main = do
    initGUI

    -- Widgets
    win <- windowNew
    box <- vBoxNew False 0
    term <- terminalNew

    -- Events
    on win objectDestroy mainQuit

    on win keyPressEvent $ do
        mods <- eventModifier
        key <- eventKeyVal

        unless (isModifierKey key) . liftIO $
            -- Handle key event
            return ()

        return True

    on term childExited $
        liftIO (putStrLn "Child exited")

    -- Layout
    containerAdd win box
    boxPackStart box term PackGrow 0

    -- Configure window
    windowSetTitle win ("Olec Text Editor" :: String)

    -- Configure terminal
    terminalSetFontFromString term ("Inconsolata 10.5" :: String)

    (ptm, pts) <- createPseudoTerminal
    terminalSetPty term (fromIntegral ptm)

    -- Display
    widgetShowAll win

    forkIO $ do
        vty <- mkVty mempty {
            inputFd = Nothing,
            outputFd = Just (Fd pts)
        }

        update vty Picture {
            picCursor = NoCursor,
            picLayers = [text' mempty "Hello World" <|> text' mempty "!"],
            picBackground = ClearBackground
        }

        putStrLn "Waiting ..."
        threadDelay 5000000

        putStrLn "Shutting down ..."
        shutdown vty
        mainQuit

    -- Run
    mainGUI
