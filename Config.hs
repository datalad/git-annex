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

{- Looks up a setting in git config. -}
getConfig :: ConfigKey -> String -> Annex String
getConfig (ConfigKey key) def = fromRepo $ Git.Config.get key def

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: ConfigKey -> String -> Annex ()
setConfig (ConfigKey key) value = do
	inRepo $ Git.Command.run "config" [Param key, Param value]
	Annex.changeGitRepo =<< inRepo Git.Config.reRead

{- Unsets a git config setting. (Leaves it in state currently.) -}
unsetConfig :: ConfigKey -> Annex ()
unsetConfig (ConfigKey key) = inRepo $ Git.Command.run "config"
	[Param "--unset", Param key]

{- A per-remote config setting in git config. -}
remoteConfig :: Git.Repo -> UnqualifiedConfigKey -> ConfigKey
remoteConfig r key = ConfigKey $
	"remote." ++ fromMaybe "" (Git.remoteName r) ++ ".annex-" ++ key

{- A global annex setting in git config. -}
annexConfig :: UnqualifiedConfigKey -> ConfigKey
annexConfig key = ConfigKey $ "annex." ++ key

{- Calculates cost for a remote. Either the specific default, or as configured 
 - by remote.<name>.annex-cost, or if remote.<name>.annex-cost-command
 - is set and prints a number, that is used. -}
remoteCost :: RemoteGitConfig -> Int -> Annex Int
remoteCost c def = case remoteAnnexCostCommand c of
	Just cmd | not (null cmd) -> liftIO $
		(fromMaybe def . readish) <$>
			readProcess "sh" ["-c", cmd]
	_ -> return $ fromMaybe def $ remoteAnnexCost c

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

getNumCopies :: Maybe Int -> Annex Int
getNumCopies (Just v) = return v
getNumCopies Nothing = annexNumCopies <$> Annex.getGitConfig

isDirect :: Annex Bool
isDirect = annexDirect <$> Annex.getGitConfig

setDirect :: Bool -> Annex ()
setDirect b = do
	setConfig (annexConfig "direct") $ if b then "true" else "false"
	Annex.changeGitConfig $ \c -> c { annexDirect = b }

{- Gets the http headers to use. -}
getHttpHeaders :: Annex [String]
getHttpHeaders = do
	v <- annexHttpHeadersCommand <$> Annex.getGitConfig
	case v of
		Just cmd -> lines <$> liftIO (readProcess "sh" ["-c", cmd])
		Nothing -> annexHttpHeaders <$> Annex.getGitConfig
