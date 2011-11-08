{- Git configuration
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config where

import Common.Annex
import qualified Git
import qualified Annex

type ConfigKey = String

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: ConfigKey -> String -> Annex ()
setConfig k value = do
	inRepo $ Git.run "config" [Param k, Param value]
	-- re-read git config and update the repo's state
	newg <- inRepo $ Git.configRead
	Annex.changeState $ \s -> s { Annex.repo = newg }

{- Looks up a per-remote config setting in git config.
 - Failing that, tries looking for a global config option. -}
getConfig :: Git.Repo -> ConfigKey -> String -> Annex String
getConfig r key def = do
	def' <- fromRepo $ Git.configGet ("annex." ++ key) def
	fromRepo $ Git.configGet (remoteConfig r key) def'

remoteConfig :: Git.Repo -> ConfigKey -> String
remoteConfig r key = "remote." ++ fromMaybe "" (Git.repoRemoteName r) ++ ".annex-" ++ key

{- Calculates cost for a remote. Either the default, or as configured 
 - by remote.<name>.annex-cost, or if remote.<name>.annex-cost-command
 - is set and prints a number, that is used.
 -}
remoteCost :: Git.Repo -> Int -> Annex Int
remoteCost r def = do
	cmd <- getConfig r "cost-command" ""
	safeparse <$> if not $ null cmd
			then liftIO $ snd <$> pipeFrom "sh" ["-c", cmd]
			else getConfig r "cost" ""
	where
		safeparse v
			| null ws = def
			| otherwise = fromMaybe def $ readMaybe $ head ws
			where
				ws = words v

cheapRemoteCost :: Int
cheapRemoteCost = 100
semiCheapRemoteCost :: Int
semiCheapRemoteCost = 110
expensiveRemoteCost :: Int
expensiveRemoteCost = 200

{- Adjust's a remote's cost to reflect it being encrypted. -}
encryptedRemoteCostAdj :: Int
encryptedRemoteCostAdj = 50

{- Make sure the remote cost numbers work out. -}
prop_cost_sane :: Bool
prop_cost_sane = False `notElem`
	[ expensiveRemoteCost > 0
	, cheapRemoteCost < semiCheapRemoteCost
	, semiCheapRemoteCost < expensiveRemoteCost
	, cheapRemoteCost + encryptedRemoteCostAdj > semiCheapRemoteCost
	, cheapRemoteCost + encryptedRemoteCostAdj < expensiveRemoteCost
	, semiCheapRemoteCost + encryptedRemoteCostAdj < expensiveRemoteCost
	]

{- Checks if a repo should be ignored, based either on annex-ignore
 - setting, or on command-line options. Allows command-line to override
 - annex-ignore. -}
repoNotIgnored :: Git.Repo -> Annex Bool
repoNotIgnored r = do
	ignored <- getConfig r "ignore" "false"
	return $ not $ Git.configTrue ignored

{- If a value is specified, it is used; otherwise the default is looked up
 - in git config. forcenumcopies overrides everything. -}
getNumCopies :: Maybe Int -> Annex Int
getNumCopies v = 
	Annex.getState Annex.forcenumcopies >>= maybe (use v) (return . id)
	where
		use (Just n) = return n
		use Nothing = read <$> fromRepo (Git.configGet config "1")
		config = "annex.numcopies"
