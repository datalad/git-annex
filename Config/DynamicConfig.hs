{- dynamic configuration
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config.DynamicConfig where

import Common

import Control.Concurrent.STM

-- | A configuration value that may only be known after performing an IO
-- action. The IO action will only be run the first time the configuration
-- is accessed; its result is then cached.
data DynamicConfig a = DynamicConfig (IO a, TMVar a) | StaticConfig a

mkDynamicConfig :: CommandRunner a -> Maybe String -> a -> STM (DynamicConfig a)
mkDynamicConfig _ Nothing static = return $ StaticConfig static
mkDynamicConfig cmdrunner (Just cmd) _ = do
	tmvar <- newEmptyTMVar
	return $ DynamicConfig (cmdrunner cmd, tmvar)

getDynamicConfig :: DynamicConfig a -> IO a
getDynamicConfig (StaticConfig v) = return v
getDynamicConfig (DynamicConfig (a, tmvar)) = 
	go =<< atomically (tryReadTMVar tmvar)
  where
	go Nothing = do
		v <- a
		atomically $ do
			_ <- tryTakeTMVar tmvar
			putTMVar tmvar v
		return v
	go (Just v) = return v

type CommandRunner a = String -> IO a

successfullCommandRunner :: CommandRunner Bool
successfullCommandRunner cmd = boolSystem "sh" [Param "-c", Param cmd]

unsuccessfullCommandRunner :: CommandRunner Bool
unsuccessfullCommandRunner cmd = not <$> successfullCommandRunner cmd

readCommandRunner :: Read a => CommandRunner (Maybe a)
readCommandRunner cmd = readish <$> readProcess "sh" ["-c", cmd]
