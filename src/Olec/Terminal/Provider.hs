module Olec.Terminal.Provider (ProviderState, targetWindow, targetState,
                               ProviderStateT,
                               Provider (..), renderProvider, renderProvider') where

import Olec.Terminal.Window
import Control.Monad.State

-- | The internal state of a Provider.
data ProviderState s = ProviderState { targetWindow :: Window
                                     , targetState  :: s }

type ProviderStateT s a = StateT (ProviderState s) IO a

-- | A Provider.
data Provider s a = Provider { runRenderer :: ProviderStateT s a }

-- | Invoke the Renderer of a Provider.
renderProvider :: Provider s a -> Window -> s -> IO (a, s)
renderProvider prov win state =
	fmap (fmap targetState) $ runStateT (runRenderer prov) (ProviderState win state)

-- | Same as renderProvider but provides the empty state.
renderProvider' :: Provider () a -> Window -> IO a
renderProvider' prov win =
	evalStateT (runRenderer prov) (ProviderState win ())
