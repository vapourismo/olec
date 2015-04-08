{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Concurrent

import Foreign.C

foreign import ccall "olec_begin"
    olecBegin :: CString -> CString -> IO CInt

main :: IO ()
main = do
    p <- withCString "Inconsolata 10.5" $
        \ font -> withCString "/tmp/olec" (olecBegin font)
    when (p == 0) $ do
        putStrLn "Hello World"
        threadDelay 10000000

    putStrLn "Terminating ..."
    return ()
