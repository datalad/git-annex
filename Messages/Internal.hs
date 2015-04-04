{- git-annex output messages
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages.Internal where

import Common
import Types
import Types.Messages
import qualified Annex

handleMessage :: IO () -> IO () -> Annex ()
handleMessage json normal = withOutputType go
  where
	go NormalOutput = liftIO normal
	go QuietOutput = q
	go ProgressOutput = q
	go JSONOutput = liftIO $ flushed json

q :: Monad m => m ()
q = noop

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout

withOutputType :: (OutputType -> Annex a) -> Annex a
withOutputType a = outputType <$> Annex.getState Annex.output >>= a
