{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface.Renderer where

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.List
import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Graphics.Text.Width

import System.IO

import Olec.Interface.Types

-- | Set the cursor position.
writeCursorPosition :: Handle -> Int -> Int -> IO ()
writeCursorPosition h x y = do
	B.hPut h "\ESC["
	hPutStr h (show (y + 1))
	B.hPut h ";"
	hPutStr h (show (x + 1))
	B.hPut h "H"

-- | Write a RGB triple.
writeRGB :: Handle -> Word8 -> Word8 -> Word8 -> IO ()
writeRGB h r g b = do
	hPutStr h (show r)
	B.hPut h ";"
	hPutStr h (show g)
	B.hPut h ";"
	hPutStr h (show b)

-- | Set the foreground color.
writeForegroundColor :: Handle -> Word8 -> Word8 -> Word8 -> IO ()
writeForegroundColor h r g b = do
	B.hPut h "\ESC[38;2;"
	writeRGB h r g b
	B.hPut h "m"

-- | Set the background color.
writeBackgroundColor :: Handle -> Word8 -> Word8 -> Word8 -> IO ()
writeBackgroundColor h r g b = do
	B.hPut h "\ESC[48;2;"
	writeRGB h r g b
	B.hPut h "m"

-- | Write Text.
writeText :: Handle -> T.Text -> IO ()
writeText h t = B.hPut h (T.encodeUtf8 t)

-- | Write String.
writeString :: Handle -> String -> IO ()
writeString = hPutStr

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
