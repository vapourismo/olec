{-# LANGUAGE ForeignFunctionInterface #-}

module Olec.Terminal (
	Terminal,
	newTerminal,
	terminalSize
) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

import Graphics.UI.Gtk.Abstract.Object
import Graphics.UI.Gtk.Abstract.Widget

import System.Glib.GObject

foreign import ccall "olec_make_vte"
	makeVTE :: CInt -> IO (Ptr Terminal)

foreign import ccall "vte_terminal_get_column_count"
	getColumnCount :: Ptr Terminal -> IO CLong

foreign import ccall "vte_terminal_get_row_count"
	getRowCount :: Ptr Terminal -> IO CLong

-- | Terminal Widget
newtype Terminal = Terminal (ForeignPtr Terminal)

instance GObjectClass Terminal where
	toGObject (Terminal ptr) = GObject (castForeignPtr ptr)
	unsafeCastGObject (GObject ptr) = Terminal (castForeignPtr ptr)

instance WidgetClass Terminal

-- | Create a new VTE instance using a pseudo-terminal master file descriptor.
newTerminal :: CInt -> IO Terminal
newTerminal ptm = makeNewObject (Terminal, objectUnref) (makeVTE ptm)

-- | Do something with the underlying raw pointer.
withTerminalPtr :: Terminal -> (Ptr Terminal -> IO a) -> IO a
withTerminalPtr (Terminal ptr) f = withForeignPtr ptr f

-- | Get the column and row count.
terminalSize :: Terminal -> IO (Int, Int)
terminalSize term =
	withTerminalPtr term $ \ ptr ->
		(,) <$> fmap fromIntegral (getColumnCount ptr) <*> fmap fromIntegral (getRowCount ptr)
