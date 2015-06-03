{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface.GTK (
	-- * Interface
	Interface,
	newInterface,
) where

import Foreign.C.Types
import Foreign.C.String

import Foreign.Ptr
import Foreign.ForeignPtr

import Control.Concurrent
import Control.Exception
import Control.Monad.State.Strict

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.General.StyleContext as G
import qualified Graphics.UI.Gtk.General.CssProvider as G

import System.Glib.GObject
import System.Posix.Types

import Olec.Visual.Image
import Olec.Visual.Types

foreign import ccall "olec_make_vte"
	makeVTE :: IO (Ptr Terminal)

foreign import ccall "vte_terminal_get_column_count"
	vteColumnCount :: Ptr Terminal -> IO CLong

foreign import ccall "vte_terminal_get_row_count"
	vteRowCount :: Ptr Terminal -> IO CLong

foreign import ccall "vte_terminal_feed"
	vteFeed :: Ptr Terminal -> CString -> CSsize -> IO ()

-- | Terminal Widget
newtype Terminal = Terminal (ForeignPtr Terminal)

instance GObjectClass Terminal where
	toGObject (Terminal ptr) = GObject (castForeignPtr ptr)
	unsafeCastGObject (GObject ptr) = Terminal (castForeignPtr ptr)

instance G.WidgetClass Terminal

-- | Create a new VTE instance.
newTerminal :: IO Terminal
newTerminal =
	makeNewGObject (Terminal, objectUnref) makeVTE

-- | Do something with the underlying raw pointer.
withTerminalPtr :: Terminal -> (Ptr Terminal -> IO a) -> IO a
withTerminalPtr (Terminal ptr) = withForeignPtr ptr

-- | Get the column and row count.
terminalSize :: Terminal -> IO (Int, Int)
terminalSize term =
	withTerminalPtr term $ \ ptr ->
		(,) <$> fmap fromIntegral (vteColumnCount ptr) <*> fmap fromIntegral (vteRowCount ptr)

-- | Feed the terminal emulator some data.
terminalFeed :: Terminal -> B.ByteString -> IO ()
terminalFeed term bs =
	withTerminalPtr term $ \ ptr ->
		B.useAsCStringLen bs $ \ (str, len) ->
			vteFeed ptr str (fromIntegral len)

-- | GTK Interface
data Interface = Interface QSem Terminal

instance Display Interface where
	dimensions (Interface _ term) =
		terminalSize term

	clear (Interface lock term) =
		bracket_ (waitQSem lock)
		         (signalQSem lock)
		         (terminalFeed term "\ESC[2J\ESC[m")

	display (Interface lock term) o i =
		bracket_ (waitQSem lock) (signalQSem lock) $ do
			-- Reset all attributes and colors
			write "\ESC[m"
			writeForeground initForeground
			writeBackground initBackground

			-- Evaluate the state transformer
			evalStateT (render o i) (Style initForeground initBackground)

		where
			initForeground :: Color
			initForeground = Color 255 255 255

			initBackground :: Color
			initBackground = Color 0 0 0

			write :: B.ByteString -> IO ()
			write = terminalFeed term

			writeMoveCursor :: Position -> IO ()
			writeMoveCursor (x, y) =
				write (B.concat ["\ESC[",
				                 BC.pack (show (y + 1)), ";",
				                 BC.pack (show (x + 1)), "H"])

			writeForeground :: Color -> IO ()
			writeForeground (Color r g b) =
				write (B.concat ["\ESC[38;2;",
				                 BC.pack (show r), ";",
				                 BC.pack (show g), ";",
				                 BC.pack (show b), "m"])

			writeBackground :: Color -> IO ()
			writeBackground (Color r g b) =
				write (B.concat ["\ESC[48;2;",
				                 BC.pack (show r), ";",
				                 BC.pack (show g), ";",
				                 BC.pack (show b), "m"])

			writeText :: T.Text -> IO ()
			writeText text =
				write (T.encodeUtf8 text)

			applyStyle :: Style -> StateT Style IO ()
			applyStyle style@(Style fg bg) = do
				Style fg' bg' <- get
				liftIO $ do
					when (fg' /= fg) (writeForeground fg)
					when (bg' /= bg) (writeBackground bg)
				put style

			render :: Position -> Image -> StateT Style IO ()
			render origin img =
				case img of
					Text style text -> do
						applyStyle style
						liftIO $ do
							writeMoveCursor origin
							writeText text

					VAlign imgs ->
						renderVertically origin imgs

					HAlign imgs ->
						renderHorizontally origin imgs

			renderVertically :: Position -> [Image] -> StateT Style IO ()
			renderVertically _ [] = pure ()
			renderVertically origin@(x, y) (img : imgs) = do
				render origin img
				renderVertically (x, y + imageHeight img) imgs

			renderHorizontally :: Position -> [Image] -> StateT Style IO ()
			renderHorizontally _ [] = pure ()
			renderHorizontally origin@(x, y) (img : imgs) = do
				render origin img
				renderHorizontally (x + imageWidth img, y) imgs

-- | Create a new GTK "Interface".
newInterface :: IO Interface
newInterface = do
	G.initGUI

	-- Remove style classes
	mbScreen <- G.screenGetDefault
	flip (maybe (return ())) mbScreen $ \ screen -> do
		cssProvider <- G.cssProviderNew
		G.cssProviderLoadFromString cssProvider ("VteTerminal { padding: 6px; }" :: T.Text)
		G.styleContextAddProviderForScreen screen cssProvider 800

	-- Main window
	win <- G.windowNew
	G.set win [G.windowTitle G.:= ("Olec Text Editor" :: T.Text)]
	G.on win G.objectDestroy G.mainQuit

	-- Box
	box <- G.vBoxNew False 0
	G.containerAdd win box

	-- Terminal
	term <- newTerminal
	G.boxPackStart box term G.PackGrow 0
	G.on term G.buttonPressEvent (return True)
	G.on term G.buttonReleaseEvent (return True)

	-- Key events
	--on term keyPressEvent $ do
	--	eval <- eventKeyVal
	--	emod <- eventModifier

	--	if isModifier eval then
	--		pure False
	--	else
	--		lift (input widget (KeyPress (toModifierMask emod) eval))

	-- Resize events
	--on term sizeAllocate (const (clearDisplay display >> update widget display))

	-- Show interface
	G.widgetShowAll win
	forkOS G.mainGUI

	-- Interface
	lock <- newQSem 1
	pure (Interface lock term)

