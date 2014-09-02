{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Olec.Terminal.Bindings (
	-- * Basics
	withTerm,
	defaultWindow,

	-- * Window Interaction
	newWindow,
	deriveWindow,
	moveWindow,
	moveDerivedWindow,
	resizeWindow,

	-- * Content Modification
	addString,
	addCString,
	refreshWindow,
) where

import Control.Exception

--import qualified Data.ByteString as B

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr


-- Initialization and finalization
foreign import ccall unsafe "olec_init"
	initTerm :: IO ()

foreign import ccall unsafe "endwin"
	disposeTerm :: IO ()

-- Default Window
foreign import ccall unsafe "olec_stdwin"
	defaultWindow_ :: IO (Ptr RawWindow)

-- Window Interaction
foreign import ccall unsafe "newwin"
	newWindow_ :: CInt -> CInt -> CInt -> CInt -> IO (Ptr RawWindow)

foreign import ccall unsafe "derwin"
	deriveWindow_ :: Ptr RawWindow -> CInt -> CInt -> CInt -> CInt -> IO (Ptr RawWindow)

foreign import ccall unsafe "&delwin"
	deleteWindow_ :: FunPtr (Ptr RawWindow -> IO ())

foreign import ccall unsafe "mvderwin"
	moveDerivedWindow_ :: Ptr RawWindow -> CInt -> CInt -> IO ()

foreign import ccall unsafe "mvwin"
	moveWindow_ :: Ptr RawWindow -> CInt -> CInt -> IO ()

foreign import ccall unsafe "wresize"
	resizeWindow_ :: Ptr RawWindow -> CInt -> CInt -> IO ()


-- Rendering
foreign import ccall unsafe "waddstr"
	addString_ :: Ptr RawWindow -> CString -> IO ()

foreign import ccall unsafe "wrefresh"
	refreshWindow_ :: Ptr RawWindow -> IO ()


data RawWindow

-- | Window Handle
type Window = ForeignPtr RawWindow


-- | Perform an IO action within an initialized terminal.
withTerm :: IO a -> IO a
withTerm = bracket_ initTerm disposeTerm

-- | Default Window
defaultWindow :: IO Window
defaultWindow = defaultWindow_ >>= newForeignPtr_

-- | Create a new window using a location and size.
newWindow :: (CInt, CInt) -> (CInt, CInt) -> IO Window
newWindow (x, y) (w, h) = newWindow_ h w y x >>= newForeignPtr deleteWindow_

-- | Create a new window by deriving from an existing one.
deriveWindow :: Window -> (CInt, CInt) -> (CInt, CInt) -> IO Window
deriveWindow win (x, y) (w, h) =
	withForeignPtr win (\t -> deriveWindow_ t h w y x) >>= newForeignPtr deleteWindow_

-- | Move a window to a different location.
moveWindow :: Window -> (CInt, CInt) -> IO ()
moveWindow win (x, y) = withForeignPtr win (\h -> moveWindow_ h y x)

-- | Move a derived window to a different location.
moveDerivedWindow :: Window -> (CInt, CInt) -> IO ()
moveDerivedWindow win (x, y) = withForeignPtr win (\h -> moveDerivedWindow_ h y x)

-- | Change the size of a window.
resizeWindow :: Window -> (CInt, CInt) -> IO ()
resizeWindow win (w, h) = withForeignPtr win (\t -> resizeWindow_ t h w)

-- | Add a string to a window.
addString :: Window -> String -> IO ()
addString win str = withForeignPtr win (withCString str . addString_)

-- | Add a C native string to a window.
addCString :: Window -> CString -> IO ()
addCString win str = withForeignPtr win (\t -> addString_ t str)

-- | Refresh the contents so the changes will be visible in the terminal.
refreshWindow :: Window -> IO ()
refreshWindow win = withForeignPtr win refreshWindow_
