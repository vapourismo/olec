{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString as B
import qualified Data.Vector.Mutable as V


type LineBuffer = V.MVector (PrimState IO)

newLineBuffer :: Int -> a -> IO (LineBuffer a)
newLineBuffer n d = do
	v <- V.new n
	forM_ [0 .. n - 1] (\m -> V.write v m d)
	return v

main :: IO ()
main = do
	v <- newLineBuffer 10 "Hello"
	n <- V.read v 0 :: IO B.ByteString

	print n
