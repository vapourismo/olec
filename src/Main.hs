{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.List

import qualified Data.Text as T

import Graphics.Text.Width

import Olec.Interface

import System.IO

-- |
type Position = (,) Int Int

-- |
type Size = (,) Int Int

-- |
data Info = Info {
	infoOutput :: Handle,
	infoOrigin :: Position,
	infoSize   :: Size
}

-- |
textWidth :: T.Text -> Int
textWidth = T.foldl' (\ n c -> n + safeWcwidth c) 0

-- |
stringWidth :: String -> Int
stringWidth = wcswidth

-- |
fitText :: Int -> T.Text -> T.Text
fitText n txt =
	snd (T.foldl' runner (n, T.empty) txt)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, T.snoc acc c)
			| otherwise = (m, acc)

-- |
fitString :: Int -> String -> String
fitString n str =
	snd (foldl' runner (n, []) str)
	where
		runner (m, acc) c
			| safeWcwidth c <= m = (m - safeWcwidth c, acc ++ [c])
			| otherwise = (m, acc)

-- |
type Renderer = ReaderT Info (StateT Position IO)

-- |
runRenderer :: Renderer a -> Handle -> Size -> IO a
runRenderer renderer out size = do
	writeCursorPosition out 0 0
	evalStateT (runReaderT renderer (Info out (0, 0) size)) (0, 0)

-- |
resetCursor :: Renderer ()
resetCursor = do
	Info out origin@(x0, y0) _ <- ask
	liftIO (writeCursorPosition out x0 y0)
	put origin

-- |
bindRenderer :: Position -> Size -> Renderer a -> Renderer a
bindRenderer (x, y) (rw, rh) renderer =
	withReaderT transform (resetCursor >> renderer)
	where
		transform (Info out (x0, y0) (w, h)) =
			let
				origin = (x0 + min (w - 1) (max 0 x),
				          y0 + min (h - 1) (max 0 y))
				size = (min (w - min (w - 1) (max 0 x)) rw,
				        min (h - min (h - 1) (max 0 y)) rh)
			in
				Info out origin size

-- |
getSize :: Renderer Size
getSize = asks infoSize

-- |
moveCursor :: Int -> Int -> Renderer ()
moveCursor x y = do
	-- Acquire information
	Info out (x0, y0) (w, h) <- ask
	pos <- get

	-- Calculate new cursor position
	let newPos@(absX, absY) = (x0 + min (w - 1) (max 0 x),
	                           y0 + min (h - 1) (max 0 y))

	-- Move cursor if new position differs from old one
	when (newPos /= pos) $ do
		liftIO (writeCursorPosition out absX absY)
		put newPos

-- |
drawText :: T.Text -> Renderer ()
drawText txt = do
	Info out (x0, _) (w, _) <- ask
	(x, _) <- get

	liftIO $
		writeText out (fitText (w - (x - x0)) txt)

-- |
drawString :: String -> Renderer ()
drawString str = do
	Info out (x0, _) (w, _) <- ask
	(x, _) <- get

	liftIO $
		writeString out (fitString (w - (x - x0)) str)

-- | Entry point
main :: IO ()
main = do
	(_, handle, sizeIO) <- makeRawInterface

	size <- sizeIO
	runRenderer render handle size
	threadDelay 2000000

	where
		render = do
			moveCursor 1 1
			size <- getSize
			drawString (show size)
