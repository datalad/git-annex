{- Git configuration
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config where

import Common.Annex
import qualified Git
import qualified Git.Config
import qualified Git.Command
import qualified Annex

type UnqualifiedConfigKey = String
data ConfigKey = ConfigKey String

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: ConfigKey -> String -> Annex ()
setConfig (ConfigKey key) value = do
	inRepo $ Git.Command.run "config" [Param key, Param value]
	Annex.changeGitRepo =<< inRepo Git.Config.reRead

{- Unsets a git config setting. (Leaves it in state currently.) -}
unsetConfig :: ConfigKey -> Annex ()
unsetConfig (ConfigKey key) = inRepo $ Git.Command.run "config"
	[Param "--unset", Param key]

{- Looks up a setting in git config. -}
getConfig :: ConfigKey -> String -> Annex String
getConfig (ConfigKey key) def = fromRepo $ Git.Config.get key def

{- Looks up a per-remote config setting in git config.
 - Failing that, tries looking for a global config option. -}
getRemoteConfig :: Git.Repo -> UnqualifiedConfigKey -> String -> Annex String
getRemoteConfig r key def = 
	getConfig (remoteConfig r key) =<< getConfig (annexConfig key) def

{- A per-remote config setting in git config. -}
remoteConfig :: Git.Repo -> UnqualifiedConfigKey -> ConfigKey
remoteConfig r key = ConfigKey $
	"remote." ++ fromMaybe "" (Git.remoteName r) ++ ".annex-" ++ key

{- A global annex setting in git config. -}
annexConfig :: UnqualifiedConfigKey -> ConfigKey
annexConfig key = ConfigKey $ "annex." ++ key

{- Calculates cost for a remote. Either the default, or as configured 
 - by remote.<name>.annex-cost, or if remote.<name>.annex-cost-command
 - is set and prints a number, that is used. -}
remoteCost :: Git.Repo -> Int -> Annex Int
remoteCost r def = do
	cmd <- getRemoteConfig r "cost-command" ""
	(fromMaybe def . readish) <$>
		if not $ null cmd
			then liftIO $ readProcess "sh" ["-c", cmd]
			else getRemoteConfig r "cost" ""

cheapRemoteCost :: Int
cheapRemoteCost = 100
semiCheapRemoteCost :: Int
semiCheapRemoteCost = 110
expensiveRemoteCost :: Int
expensiveRemoteCost = 200
veryExpensiveRemoteCost :: Int
veryExpensiveRemoteCost = 1000

{- Adjusts a remote's cost to reflect it being encrypted. -}
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

{- Checks if a repo should be ignored. -}
repoNotIgnored :: Git.Repo -> Annex Bool
repoNotIgnored r = not . fromMaybe False . Git.Config.isTrue
	<$> getRemoteConfig r "ignore" ""

{- Checks if a repo should be synced. -}
repoSyncable :: Git.Repo -> Annex Bool
repoSyncable r = fromMaybe True . Git.Config.isTrue
	<$> getRemoteConfig r "sync" ""

{- Gets the trust level set for a remote in git config. -}
getTrustLevel :: Git.Repo -> Annex (Maybe String)
getTrustLevel r = fromRepo $ Git.Config.getMaybe key
  where
	(ConfigKey key) = remoteConfig r "trustlevel"

getNumCopies :: Maybe Int -> Annex Int
getNumCopies (Just v) = return v
getNumCopies Nothing = annexNumCopies <$> Annex.getConfig

isDirect :: Annex Bool
isDirect = annexDirect <$> Annex.getConfig

setDirect :: Bool -> Annex ()
setDirect b = do
	setConfig (annexConfig "direct") $ if b then "true" else "false"
	Annex.changeConfig $ \c -> c { annexDirect = b }

{- Gets the http headers to use. -}
getHttpHeaders :: Annex [String]
getHttpHeaders = do
	v <- annexHttpHeadersCommand <$> Annex.getConfig
	case v of
		Just cmd -> lines <$> liftIO (readProcess "sh" ["-c", cmd])
		Nothing -> annexHttpHeaders <$> Annex.getConfig
