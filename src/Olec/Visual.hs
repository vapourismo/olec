module Olec.Visual (
	Bounds,
	Box (..),
	Canvas (..),
	Render (..)
) where

import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString as B


type Bounds = (CInt, CInt, CInt, CInt)


class Box a where
	getBounds :: a -> IO Bounds
	setBounds :: a -> Bounds -> IO ()

class Box a => Canvas a where
	setCursor :: a -> (CInt, CInt) -> IO ()
	getCursor :: a -> IO (CInt, CInt)
	drawCString :: a -> CString -> IO ()
	clear :: a -> IO ()

	drawString :: a -> String -> IO ()
	drawString c s = withCString s (drawCString c)

	drawByteString :: a -> B.ByteString -> IO ()
	drawByteString c b = B.useAsCString b (drawCString c)

class Box a => Render a where
	render :: a -> IO ()
