import Olec.Terminal
import Olec.Terminal.Input

import Control.Monad.State
import Control.Concurrent
import Control.Lens

type AppStateT s = StateT s IO

--processInputForever :: (KeyStroke -> AppStateT s Bool) -> AppStateT s ()
--processInputForever hook = do
--	(q, t) <- liftIO $ do
--		q <- newChan
--		t <- forkIO (inputWorker q)
--		return (q, t)

--	let looper = do
--		k <- liftIO (readChan q)
--		cont <- hook k
--		when cont looper

--	looper
--	liftIO (killThread t)

main = withTerm $ do
	src <- processInput
	render
	readKeyStroke src
