{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import Data.IORef

import Graphics.UI.Gtk

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable

import System.Glib.GObject

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

--foreign import ccall unsafe "forkpty"
--    forkpty :: Ptr CInt -> Ptr () -> Ptr () -> Ptr () -> IO CPid

foreign import ccall unsafe "olec_make_vte"
    makeVTE :: CInt -> IO (Ptr GObject)

foreign import ccall unsafe "vte_terminal_get_column_count"
    countCols :: Ptr GObject -> IO CLong

foreign import ccall unsafe "vte_terminal_get_row_count"
    countRows :: Ptr GObject -> IO CLong

newTerminal :: CInt -> IO Widget
newTerminal ptm = do
    raw <- makeVTE ptm
    objectRef raw

    ptr <- newForeignPtr objectUnref raw
    return (castToWidget (GObject ptr))

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

    on win keyPressEvent $ do
        eval <- eventKeyVal
        emod <- eventModifier

        unless (isModifierKey eval) . lift $ do
            writeChan eventChan (KeyPress emod eval)

        return True

    -- Box
    box <- vBoxNew False 0
    containerAdd win box

    -- Terminal
    (ptm, _) <- createPseudoTerminal
    term <- newTerminal ptm
    boxPackStart box term PackGrow 0

    dimRef <- newIORef (0, 0)
    on term sizeAllocate $ \ _ ->
        let GObject ptr = toGObject term
        in withForeignPtr ptr $ \ raw -> do
            tup <- (,) <$> countCols raw <*> countRows raw
            tup' <- readIORef dimRef
            when (tup /= tup') $ do
                writeIORef dimRef tup
                writeChan eventChan (uncurry Resize tup)

    -- Dispatch events
    forkIO (forever (readChan eventChan >>= print))

    -- Run
    widgetShowAll win
    mainGUI
