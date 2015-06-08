{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface.GTK (
	-- * Interface
	Interface,
	newInterface,
	runInterface,
	exitInterface,

	-- * Events
	registerKeyHandler,
	registerResizeHandler
) where

import Foreign.C.Types
import Foreign.C.String

import Foreign.Ptr
import Foreign.ForeignPtr

import Control.Concurrent

import Control.Monad
import Control.Monad.Trans

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.General.StyleContext as G
import qualified Graphics.UI.Gtk.General.CssProvider as G

import System.Glib.GObject
import System.Posix.Types

import Olec.Event.Keys
import Olec.Visual

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
withTerminalPtr (Terminal ptr) =
	withForeignPtr ptr

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

instance Canvas Interface where
	lockCanvas (Interface lock _) =
		waitQSem lock

	unlockCanvas (Interface lock _) =
		signalQSem lock

	sizeOfCanvas (Interface _ term) =
		terminalSize term

	clearCanvas (Interface _ term) =
		terminalFeed term "\ESC[2J\ESC[m"

	hideCursor (Interface _ term) =
		terminalFeed term "\ESC[?25l"

	showCursor (Interface _ term) =
		terminalFeed term "\ESC[?25h"

	setForeground (Interface _ term) (Color r g b) =
		terminalFeed term (B.concat ["\ESC[38;2;",
		                             BC.pack (show r), ";",
		                             BC.pack (show g), ";",
		                             BC.pack (show b), "m"])

	setBackground (Interface _ term) (Color r g b) =
		terminalFeed term (B.concat ["\ESC[48;2;",
		                             BC.pack (show r), ";",
		                             BC.pack (show g), ";",
		                             BC.pack (show b), "m"])

	drawText (Interface _ term) txt =
		terminalFeed term (T.encodeUtf8 txt)

	moveCursor (Interface _ term) (x, y) =
		terminalFeed term (B.concat ["\ESC[",
		                             BC.pack (show (y + 1)), ";",
		                             BC.pack (show (x + 1)), "H"])


-- | Register a callback which will be called upon receiving a key event.
registerKeyHandler :: Interface -> (KeyEvent -> IO ()) -> IO ()
registerKeyHandler (Interface _ term) handler =
	void $ G.on term G.keyPressEvent $ do
		eval <- G.eventKeyVal
		emod <- G.eventModifier

		unless (isModifier eval) $
			liftIO (handler (KeyEvent (toModifierMask emod) eval))

		pure True
	where
		isModifier key =
			(0xffe1 <= key && key <= 0xffee) ||
			(0xfe01 <= key && key <= 0xfe0f) ||
			(0xfe11 <= key && key <= 0xfe13) ||
			key == 0xff7e

-- | Register a callback which will be called upon receiving a resize event.
registerResizeHandler :: Interface -> (Size -> IO ()) -> IO ()
registerResizeHandler (Interface _ term) handler =
	void (G.on term G.sizeAllocate (const (terminalSize term >>= handler)))

-- | Create a new GTK "Interface".
newInterface :: IO Interface
newInterface = do
	G.initGUI

	-- Remove style classes
	mbScreen <- G.screenGetDefault
	flip (maybe (pure ())) mbScreen $ \ screen -> do
		cssProvider <- G.cssProviderNew
		G.cssProviderLoadFromString cssProvider ("VteTerminal { padding: 0px; }" :: T.Text)
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
	G.on term G.buttonPressEvent (pure True)
	G.on term G.buttonReleaseEvent (pure True)

	-- Show interface
	G.widgetShowAll win

	-- Interface
	lock <- newQSem 1
	pure (Interface lock term)

-- | Start the GTK main loop
runInterface :: IO ()
runInterface = G.mainGUI

-- | Exit the GTK main loop
exitInterface :: IO ()
exitInterface = G.mainQuit
