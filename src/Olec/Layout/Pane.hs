module Olec.Layout.Pane where

import Control.Applicative
import Data.Word
import Olec.Terminal


-- | A cat.
data Pane
	= Pane { paneOrigin :: (Word, Word)
	       , paneSize   :: (Word, Word) }

-- | Root pane.
rootPane :: IO Pane
rootPane = Pane (0, 0) <$> termSize
