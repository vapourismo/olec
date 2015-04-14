{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (main) where

--import Control.Monad
--import Control.Concurrent

--import qualified Data.ByteString as B

--import Foreign.C
--import Foreign.Marshal
--import Foreign.Ptr

--import System.Posix.IO
--import System.Posix.Types

--import Graphics.Vty

--foreign import ccall unsafe "olec_begin"
--    olecBegin_ :: CString -> Ptr CString -> CInt -> IO CInt

--foreign import ccall unsafe "olec_debug"
--    olecDebug :: IO ()

--foreign import ccall unsafe "gtk_main"
--    gtkMain :: IO ()

--data Configuration = Configuration {
--    confFontDescription :: B.ByteString,
--    confColorPalette :: [B.ByteString]
--}

--useAsCStrings :: [B.ByteString] -> ([CString] -> IO a) -> IO a
--useAsCStrings rbs f = impl (reverse rbs) [] where
--    impl [] cs = f cs
--    impl (b : bs) cs =
--        B.useAsCString b (\ c -> impl bs (c : cs))

--olecBegin :: Configuration -> IO CInt
--olecBegin Configuration {..} =
--    useAsCStrings palette $ \ cs ->
--        withArray cs $ \ palettePtr ->
--            B.useAsCString confFontDescription $ \ fontDescr ->
--                olecBegin_ fontDescr palettePtr 256
--    where
--        palette =
--            if length confColorPalette < 256 then
--                confColorPalette ++ replicate (256 - length confColorPalette) "#ffffff"
--            else
--                take 256 confColorPalette

--main :: IO ()
--main = do
--    pts <- olecBegin Configuration {
--        confFontDescription = "Inconsolata 10.5",
--        confColorPalette = [
--            "#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55",
--            "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5"
--        ]
--    }

--    --vty <- mkVty mempty {
--    --    inputFd = Just (Fd pts),
--    --    outputFd = Just (Fd pts),
--    --    termName = Just "xterm-256color"
--    --}

--    --update vty Picture {
--    --    picCursor = Cursor 10 10,
--    --    picLayers = [text' (withForeColor mempty (ISOColor 2)) "Hello World"],
--    --    picBackground = ClearBackground
--    --}

--    --refresh vty
--    --fdWrite (Fd pts) ""

--    forkOS gtkMain

--    forever $ do
--        olecDebug
--        threadDelay 1000000

--    --shutdown vty

import Control.Monad.Reader
import Graphics.UI.Gtk


isModifierKey :: KeyVal -> Bool
isModifierKey key =
    (0xffe1 <= key && key <= 0xffee) ||
    (0xfe01 <= key && key <= 0xfe0f) ||
    (0xfe11 <= key && key <= 0xfe13) ||
    key == 0xff7e

main :: IO ()
main = do
    initGUI

    win <- windowNew

    on win objectDestroy mainQuit
    on win keyPressEvent $ do
        mods <- eventModifier
        key <- eventKeyVal

        unless (isModifierKey key) . liftIO $ do
            print (mods, key)
        return True

    widgetShowAll win
    mainGUI
