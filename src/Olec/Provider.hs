module Olec.Provider (
	Provider (..),
	updateProvider,
	renderProvider
) where

import Olec.Terminal
import Olec.Terminal.Window

-- | Provider
data Provider s a
	= Provider s (Window -> s -> s) (Window -> s -> IO a)

-- | Update a provider.
updateProvider :: Provider s a -> Window -> Provider s a
updateProvider (Provider s u r) w = Provider (u w s) u r

-- | Render a provider.
renderProvider :: Provider s a -> Window -> IO a
renderProvider (Provider s _ r) w = do
	x <- r w s
	render
	return x
