{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Control.Monad
import Control.Concurrent

import qualified Data.ByteString as B

import Foreign.C
import Foreign.Marshal
import Foreign.Ptr

import Graphics.Vty

foreign import ccall "olec_begin"
    olecBegin_ :: CString -> Ptr CString -> CInt -> IO CInt

data Configuration = Configuration {
    confFontDescription :: B.ByteString,
    confColorPalette :: [B.ByteString]
}

useAsCStrings :: [B.ByteString] -> ([CString] -> IO a) -> IO a
useAsCStrings rbs f = impl (reverse rbs) [] where
    impl [] cs = f cs
    impl (b : bs) cs =
        B.useAsCString b (\ c -> impl bs (c : cs))

olecBegin :: Configuration -> IO CInt
olecBegin Configuration {..} =
    useAsCStrings palette $ \ cs ->
        withArray cs $ \ palettePtr ->
            B.useAsCString confFontDescription $ \ fontDescr ->
                olecBegin_ fontDescr palettePtr 256
    where
        palette =
            if length confColorPalette < 256 then
                confColorPalette ++ replicate (256 - length confColorPalette) "#ffffff"
            else
                take 256 confColorPalette

main :: IO ()
main = do
    p <- olecBegin Configuration {
        confFontDescription = "Inconsolata 10.5",
        confColorPalette = [
            "#1A1A1A", "#D9715F", "#B2CC46", "#FFCB55",
            "#6486BC", "#AD7FA8", "#06989A", "#D5D5D5"
        ]
    }

    when (p == 0) $ do
        ui <- mkVty mempty {
            termName = Just "xterm-256color"
        }

        update ui emptyPicture
        --refresh ui
        nextEvent ui
        shutdown ui

    putStrLn "Terminating ..."
    return ()
