--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

--import Control.Concurrent

--import qualified Data.Text.Encoding as T

--import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as BC

--import System.IO

--import Olec.Visual.Types
--import Olec.Visual.Image

---- |
--newtype VT100 = VT100 B.ByteString
--	deriving (Monoid)

---- |
--instance ImageIR VT100 where
--	mkSetForeground (Color r g b) =
--		VT100 (B.concat ["\ESC[38;2;",
--		                 BC.pack (show r), ";",
--		                 BC.pack (show g), ";",
--		                 BC.pack (show b), "m"])

--	mkSetBackground (Color r g b) =
--		VT100 (B.concat ["\ESC[48;2;",
--		                 BC.pack (show r), ";",
--		                 BC.pack (show g), ";",
--		                 BC.pack (show b), "m"])

--	mkMoveCursor (x, y) =
--		VT100 (B.concat ["\ESC[",
--		                 BC.pack (show (y + 1)), ";",
--		                 BC.pack (show (x + 1)), "H"])

--	mkText = VT100 . T.encodeUtf8

---- |
--main :: IO ()
--main = do
--	img <- paintImage (text (Style "#ffffff" "#000000") "Hello World") (20, 20)
--	let VT100 bs = toImageIR (2, 2) img

--	B.hPut stdout bs
--	threadDelay 5000000

import Control.Monad.Reader
import Control.Monad.State.Strict

import Graphics.UI.Gtk hiding (moveCursor, rectangle)
import Graphics.Rendering.Cairo

drawLayout :: PangoContext -> String -> Render ()
drawLayout context txt = do
	layout <- liftIO (layoutText context txt)
	updateLayout layout
	showLayout layout

data RenderInfo = RenderInfo {
	rcPangoContext :: PangoContext,
	rcCharWidth    :: Double,
	rcCharHeight   :: Double,
	rcColumns      :: Int,
	rcRows         :: Int,
	rcXOffset      :: Double,
	rcYOffset      :: Double
}

mkRenderContext :: PangoContext -> FontDescription -> Render RenderInfo
mkRenderContext context font = do
	FontMetrics a d charWidth _ _ _ _ _ <- liftIO (contextGetMetrics context font emptyLanguage)
	let charHeight = a + d

	(_, _, clipWidth, clipHeight) <- clipExtents

	let maxCols = floor (clipWidth / charWidth) :: Int
	let maxRows = floor (clipHeight / charHeight) :: Int

	let xPadding = (clipWidth - fromIntegral maxCols * charWidth) / 2
	let yPadding = (clipHeight - fromIntegral maxRows * charHeight) / 2

	pure (RenderInfo context
	                 charWidth charHeight
	                 maxCols maxRows
	                 xPadding yPadding)

data RenderState = RenderState

newtype CairoPainter a = CairoPainter (ReaderT RenderInfo (StateT RenderState Render) a)
	deriving (Functor, Applicative, Monad, MonadReader RenderInfo,
	          MonadState RenderState, MonadIO)

instance (Monoid a) => Monoid (CairoPainter a) where
	mempty = pure mempty
	mappend a b = mappend <$> a <*> b
	mconcat xs = mconcat <$> sequence xs

runCairoPainter :: PangoContext -> FontDescription -> CairoPainter a -> Render a
runCairoPainter context font (CairoPainter cp) = do
	rc <- mkRenderContext context font
	evalStateT (runReaderT cp rc) RenderState

moveCursor :: Int -> Int -> CairoPainter ()
moveCursor x y = do
	RenderInfo {..} <- ask
	CairoPainter $ lift $ lift $
		moveTo (rcXOffset + fromIntegral x * rcCharWidth)
		       (rcYOffset + fromIntegral y * rcCharHeight)

drawString :: String -> CairoPainter ()
drawString str = do
	RenderInfo {..} <- ask
	layout <- liftIO (layoutText rcPangoContext str)
	CairoPainter $ lift $ lift $ do
		updateLayout layout
		showLayout layout

main :: IO ()
main = do
	initGUI
	window <- windowNew
	drawingArea <- drawingAreaNew
	containerAdd window drawingArea

	context <- cairoCreateContext Nothing
	font <- fontDescriptionFromString "Inconsolata 10.5"
	contextSetFontDescription context font

	on drawingArea draw $ do
		-- Background
		(clipX, clipY, clipWidth, clipHeight) <- clipExtents
		rectangle clipX clipY clipWidth clipHeight
		setSourceRGB (26 / 255) (26 / 255) (26 / 255)
		fill

		-- Information
		setSourceRGB (213 / 255) (213 / 255) (213 / 255)
		runCairoPainter context font $ do
			moveCursor 0 0
			drawString "Hello World"

			moveCursor 5 1
			drawString " World"

		pure ()

	on window objectDestroy mainQuit

	windowSetDefaultSize window 640 480
	widgetShowAll window
	mainGUI
