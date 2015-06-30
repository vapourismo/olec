{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Olec.Visual.CairoIR (
	CairoIR,
	toCairoRender,
	cairoClipSize
) where

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

data RenderState = RenderState Color Color

-- | Intermediate representation
newtype CairoIR a = CairoIR (ReaderT RenderInfo (StateT RenderState C.Render) a)
	deriving (Functor, Applicative, Monad, MonadIO,
	          MonadReader RenderInfo, MonadState RenderState)

instance (Monoid a) => Monoid (CairoIR a) where
	mempty = pure mempty
	mappend a b = mappend <$> a <*> b
	mconcat xs = mconcat <$> sequence xs

instance ImageIR (CairoIR ()) where
	mkSetForeground fg =
		modify (\ (RenderState _ bg) -> RenderState fg bg)

	mkSetBackground bg =
		modify (\ (RenderState fg _) -> RenderState fg bg)

	mkMoveCursor (x, y) = do
		RenderInfo {..} <- ask
		CairoIR $ lift $ lift $
			C.moveTo (riXOffset + fromIntegral x * riCharWidth)
			         (riYOffset + fromIntegral y * riCharHeight)

	mkText txt = do
		RenderInfo {..} <- ask
		RenderState (Color fgR fgG fgB) (Color bgR bgG bgB) <- get

		(layout, w, h) <- liftIO $ do
			layout <- G.layoutText riPangoContext txt
			(_, G.PangoRectangle _ _ w h) <- G.layoutGetExtents layout
			pure (layout, w, h)

		CairoIR $ lift $ lift $ do
			-- Draw background
			C.setSourceRGB (fromIntegral bgR / 255)
			               (fromIntegral bgG / 255)
			               (fromIntegral bgB / 255)

			(x, y) <- C.getCurrentPoint

			C.rectangle x y w h
			C.strokePreserve
			C.fill

			C.moveTo x y

			-- Draw foreground
			C.setSourceRGB (fromIntegral fgR / 255)
			               (fromIntegral fgG / 255)
			               (fromIntegral fgB / 255)

			G.updateLayout layout
			G.showLayout layout

-- | Convert to cairo render.
toCairoRender :: G.PangoContext -> CairoIR a -> C.Render a
toCairoRender context (CairoIR cp) = do
	ri <- mkRenderInfo context
	evalStateT (runReaderT cp ri) (RenderState (Color 0 0 0) (Color 0 0 0))

-- | Get clip size.
cairoClipSize :: CairoIR (Int, Int)
cairoClipSize = asks (\ RenderInfo {..} -> (riColumns, riRows))
