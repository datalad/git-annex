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
import Utility.DataUnits

type UnqualifiedConfigKey = String
data ConfigKey = ConfigKey String

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: ConfigKey -> String -> Annex ()
setConfig (ConfigKey key) value = do
	inRepo $ Git.Command.run "config" [Param key, Param value]
	newg <- inRepo Git.Config.reRead
	Annex.changeState $ \s -> s { Annex.repo = newg }

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
			then liftIO $ snd <$> pipeFrom "sh" ["-c", cmd]
			else getRemoteConfig r "cost" ""

cheapRemoteCost :: Int
cheapRemoteCost = 100
semiCheapRemoteCost :: Int
semiCheapRemoteCost = 110
expensiveRemoteCost :: Int
expensiveRemoteCost = 200

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

{- If a value is specified, it is used; otherwise the default is looked up
 - in git config. forcenumcopies overrides everything. -}
getNumCopies :: Maybe Int -> Annex Int
getNumCopies v = perhaps (use v) =<< Annex.getState Annex.forcenumcopies
	where
		use (Just n) = return n
		use Nothing = perhaps (return 1) =<< 
			readish <$> getConfig (annexConfig "numcopies") "1"
		perhaps fallback = maybe fallback (return . id)

{- Gets the trust level set for a remote in git config. -}
getTrustLevel :: Git.Repo -> Annex (Maybe String)
getTrustLevel r = fromRepo $ Git.Config.getMaybe key
	where
		(ConfigKey key) = remoteConfig r "trustlevel"

{- Gets annex.diskreserve setting. -}
getDiskReserve :: Annex Integer
getDiskReserve = fromMaybe megabyte . readSize dataUnits
	<$> getConfig (annexConfig "diskreserve") ""
	where
		megabyte = 1000000

{- Gets annex.httpheaders or annex.httpheaders-command setting,
 - splitting it into lines. -}
getHttpHeaders :: Annex [String]
getHttpHeaders = do
	cmd <- getConfig (annexConfig "http-headers-command") ""
	if (null cmd)
		then fromRepo $ Git.Config.getList "annex.http-headers"
		else lines . snd <$> liftIO (pipeFrom "sh" ["-c", cmd])
