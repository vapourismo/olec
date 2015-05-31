{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Interface.Terminal (
	Terminal,
	newTerminal,
	terminalSize,
	terminalFeed
) where

import Foreign.C.Types
import Foreign.C.String

import Foreign.Ptr
import Foreign.ForeignPtr

import qualified Data.ByteString as B

import Graphics.UI.Gtk.Abstract.Object
import Graphics.UI.Gtk.Abstract.Widget

import System.Glib.GObject
import System.Posix.Types

foreign import ccall "olec_make_vte"
	makeVTE :: IO (Ptr Terminal)

foreign import ccall "vte_terminal_get_column_count"
	getColumnCount :: Ptr Terminal -> IO CLong

foreign import ccall "vte_terminal_get_row_count"
	getRowCount :: Ptr Terminal -> IO CLong

foreign import ccall "vte_terminal_feed"
	feedData :: Ptr Terminal -> CString -> CSsize -> IO ()

-- | Terminal Widget
newtype Terminal = Terminal (ForeignPtr Terminal)

instance GObjectClass Terminal where
	toGObject (Terminal ptr) = GObject (castForeignPtr ptr)
	unsafeCastGObject (GObject ptr) = Terminal (castForeignPtr ptr)

instance WidgetClass Terminal

-- | Create a new VTE instance using a pseudo-terminal master file descriptor.
newTerminal :: IO Terminal
newTerminal =
	makeNewObject (Terminal, objectUnref) makeVTE

-- | Do something with the underlying raw pointer.
withTerminalPtr :: Terminal -> (Ptr Terminal -> IO a) -> IO a
withTerminalPtr (Terminal ptr) = withForeignPtr ptr

-- | Get the column and row count.
terminalSize :: Terminal -> IO (Int, Int)
terminalSize term =
	withTerminalPtr term $ \ ptr ->
		(,) <$> fmap fromIntegral (getColumnCount ptr) <*> fmap fromIntegral (getRowCount ptr)

-- | Feed the terminal emulator some data.
terminalFeed :: Terminal -> B.ByteString -> IO ()
terminalFeed term bs =
	withTerminalPtr term $ \ ptr ->
		B.useAsCStringLen bs $ \ (str, len) ->
			feedData ptr str (fromIntegral len)
