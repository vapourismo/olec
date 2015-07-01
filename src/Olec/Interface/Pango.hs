module Olec.Interface.Pango (
	-- * Interface
	Interface,
	newInterface,
	runInterface,
	exitInterface,

	-- * Interaction
	requestDraw,
	setFont,
	setPainter
) where

import Control.Monad
import Control.Monad.Trans

import qualified Data.Text as T

import Graphics.UI.Gtk hiding (Color, rectangle)
import Graphics.Rendering.Cairo

import Olec.Visual.Types
import Olec.Visual.Image
import Olec.Visual.CairoIR

data Interface = Interface Window DrawingArea PangoContext

newInterface :: IO Interface
newInterface = do
	initGUI

	window <- windowNew
	drawingArea <- drawingAreaNew
	containerAdd window drawingArea

	on window objectDestroy mainQuit

	widgetShowAll window

	Interface window drawingArea <$> cairoCreateContext Nothing

runInterface :: IO ()
runInterface = mainGUI

exitInterface :: IO ()
exitInterface = mainQuit

requestDraw :: Interface -> IO ()
requestDraw (Interface _ drawingArea _) =
	widgetQueueDraw drawingArea

setFont :: Interface -> T.Text -> IO ()
setFont (Interface _ _ context) font =
	fontDescriptionFromString font >>= contextSetFontDescription context

fillBackground :: Color -> Render ()
fillBackground (Color r g b) = do
	(clipX, clipY, clipWidth, clipHeight) <- clipExtents
	rectangle clipX clipY clipWidth clipHeight
	setSourceRGB (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255)
	fill

setPainter :: Interface -> Color -> Painter -> IO ()
setPainter (Interface _ drawingArea context) background painter =
	void $ on drawingArea draw $ do
		setAntialias AntialiasSubpixel
		setLineWidth 1

		fillBackground background
		toCairoRender context $ do
			size <- cairoClipSize
			liftIO (paintImage size painter) >>= toImageIR (0, 0)
