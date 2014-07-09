{-# LANGUAGE ExistentialQuantification #-}

module Olec.Component (
	-- * Component
	Com (..),
	drawCom,
	updateCom,

	-- * Useful Components
	mkSplitCom,
) where

import Olec.Window
import Control.Applicative


-- | Component
data Com s
	= forall a. Com s                -- ^ Internal State
	                (s -> Update a)  -- ^ Renderer
	                (s -> Update s)  -- ^ Updater

-- | Draw a component.
drawCom :: Com s -> Update ()
drawCom (Com s r _) = () <$ r s

-- | Update a component.
updateCom :: Com s -> Update (Com s)
updateCom (Com s r u) = (\s' -> Com s' r u) <$> u s


-- | Splitter Component
type SplitCom s t = Com (SplitInfo, Com s, Com t, Window, Window)

-- | Make a splitter component.
mkSplitCom :: SplitInfo -> Com s -> Com t -> Update (SplitCom s t)
mkSplitCom info a b = do
	(wa, wb) <- splitWindow info
	return $ Com (info, a, b, wa, wb) render update where
		-- Update function
		update (info, a, b, _, _) = do
			(wa, wb) <- splitWindow info
			a' <- withWindow wa (updateCom a) -- Update A
			b' <- withWindow wb (updateCom b) -- Update B
			return (info, a', b', wa, wb)

		-- Render function
		render (_, a, b, wa, wb) = do
			withWindow wa (drawCom a) -- Render A
			withWindow wb (drawCom b) -- Render B
