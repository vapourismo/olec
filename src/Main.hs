{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C

import Olec.Visual.Types
import Olec.Visual.Image

data RenderInfo = RenderInfo {
	riPangoContext :: G.PangoContext,
	riCharWidth    :: Double,
	riCharHeight   :: Double,
	riColumns      :: Int,
	riRows         :: Int,
	riXOffset      :: Double,
	riYOffset      :: Double
}

mkRenderInfo :: G.PangoContext -> C.Render RenderInfo
mkRenderInfo context = do
	G.FontMetrics a d charWidth _ _ _ _ _ <- liftIO $ do
		G.contextSetTextGravityHint context G.PangoGravityHintLine

		font <- G.contextGetFontDescription context
		lang <- G.contextGetLanguage context
		G.contextGetMetrics context font lang

	let charHeight = a + d

	(_, _, clipWidth, clipHeight) <- C.clipExtents

	let maxCols = floor (clipWidth / charWidth) :: Int
	let maxRows = floor (clipHeight / charHeight) :: Int

	let xPadding = (clipWidth - fromIntegral maxCols * charWidth) / 2
	let yPadding = (clipHeight - fromIntegral maxRows * charHeight) / 2

	pure (RenderInfo context
	                 charWidth charHeight
	                 maxCols maxRows
	                 xPadding yPadding)

data RenderState = RenderState {
	rsForeground :: Color,
	rsBackground :: Color
}

newtype CairoPainter a = CairoPainter (ReaderT RenderInfo (StateT RenderState C.Render) a)
	deriving (Functor, Applicative, Monad, MonadReader RenderInfo,
	          MonadState RenderState, MonadIO)

instance (Monoid a) => Monoid (CairoPainter a) where
	mempty = pure mempty
	mappend a b = mappend <$> a <*> b
	mconcat xs = mconcat <$> sequence xs

instance ImageIR (CairoPainter ()) where
	mkSetForeground fg =
		modify (\ (RenderState _ bg) -> RenderState fg bg)

	mkSetBackground bg =
		modify (\ (RenderState fg _) -> RenderState fg bg)

	mkMoveCursor (x, y) = do
		RenderInfo {..} <- ask
		CairoPainter $ lift $ lift $
			C.moveTo (riXOffset + fromIntegral x * riCharWidth)
			         (riYOffset + fromIntegral y * riCharHeight)

	mkText txt = do
		RenderInfo {..} <- ask
		RenderState (Color fgR fgG fgB) (Color bgR bgG bgB) <- get

		layout <- liftIO (G.layoutText riPangoContext txt)

		CairoPainter $ lift $ lift $ do
			-- Draw background
			C.setSourceRGB (fromIntegral bgR / 255)
			               (fromIntegral bgG / 255)
			               (fromIntegral bgB / 255)

			(x, y) <- C.getCurrentPoint
			C.rectangle x y (fromIntegral (textWidth txt) * riCharWidth) riCharHeight
			C.fill

			C.moveTo x y

			-- Draw foreground
			C.setSourceRGB (fromIntegral fgR / 255)
			               (fromIntegral fgG / 255)
			               (fromIntegral fgB / 255)

			G.updateLayout layout
			G.showLayout layout

runCairoPainter :: G.PangoContext -> CairoPainter a -> C.Render a
runCairoPainter context (CairoPainter cp) = do
	ri <- mkRenderInfo context
	evalStateT (runReaderT cp ri) (RenderState (Color 0 0 0) (Color 0 0 0))

getCanvasSize :: CairoPainter (Int, Int)
getCanvasSize = asks (\ RenderInfo {..} -> (riColumns, riRows))

main :: IO ()
main = do
	G.initGUI
	window <- G.windowNew
	drawingArea <- G.drawingAreaNew
	G.containerAdd window drawingArea

	context <- G.cairoCreateContext Nothing
	font <- G.fontDescriptionFromString ("Inconsolata 10.5" :: String)
	G.contextSetFontDescription context font

	G.on drawingArea G.draw $ do
		-- Background
		(clipX, clipY, clipWidth, clipHeight) <- C.clipExtents
		C.rectangle clipX clipY clipWidth clipHeight
		C.setSourceRGB (26 / 255) (26 / 255) (26 / 255)
		C.fill

		runCairoPainter context $ do
			let painter =
				center $
					vcat [text (Style "#ff0000" "#00ff00") "Hello World",
	                      text (Style "#ff0000" "#00ff00") "Hello World"]

			size <- getCanvasSize
			img <- liftIO (paintImage size painter)
			toImageIR (0, 0) img :: CairoPainter ()

		pure ()

	G.on window G.objectDestroy G.mainQuit

	G.windowSetDefaultSize window 640 480
	G.widgetShowAll window
	G.mainGUI
