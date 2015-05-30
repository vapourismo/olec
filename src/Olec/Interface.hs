{-# LANGUAGE OverloadedStrings #-}

module Olec.Interface (
	-- * Initializers
	makeInterface,

	-- * Interaction
	writeCursorPosition,
	writeForegroundColor,
	writeBackgroundColor,
	writeText,
	writeString,

	-- * Re-exports
	module Olec.Interface.Events,
	module Olec.Interface.Types
) where

import Control.Exception
import Control.Concurrent

import Data.Word

import qualified Data.ByteString as B

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Graphics.UI.Gtk hiding (Size)
import Graphics.UI.Gtk.General.StyleContext
import Graphics.UI.Gtk.General.CssProvider

import System.IO

import System.Posix.IO
import System.Posix.Types
import System.Posix.Terminal

import Olec.Interface.Events
import Olec.Interface.Terminal
import Olec.Interface.Types

-- | Launch user interface.
launchUI :: Chan Event -> IO (Fd, IO Size)
launchUI eventChan = do
	initGUI

	-- Remove style classes
	mbScreen <- screenGetDefault
	flip (maybe (return ())) mbScreen $ \ screen -> do
		cssProvider <- cssProviderNew
		cssProviderLoadFromString cssProvider ("VteTerminal { padding: 6px; }" :: T.Text)
		styleContextAddProviderForScreen screen cssProvider 800

	-- Main window
	win <- windowNew
	set win [windowTitle := ("Olec Text Editor" :: T.Text)]

	-- Box
	box <- vBoxNew False 0
	containerAdd win box

	-- Terminal
	(Fd ptm, pts) <- openPseudoTerminal
	term <- newTerminal ptm ["#111111"]
	boxPackStart box term PackGrow 0

	-- Dispatch events
	forwardKeyPressEvents win eventChan
	forwardResizeEvents term eventChan

	on win objectDestroy mainQuit
	on term buttonPressEvent (return True)
	on term buttonReleaseEvent (return True)

	-- Show interface
	widgetShowAll win
	forkOS (finally mainGUI (writeChan eventChan ExitRequest))

	pure (pts, terminalSize term)

-- | Create the main user interface.
makeInterface :: IO (Chan Event, Handle, IO Size)
makeInterface = do
	eventChan <- newChan
	(pts, sizeAction) <- launchUI eventChan
	output <- fdToHandle pts

	pure (eventChan, output, sizeAction)

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
