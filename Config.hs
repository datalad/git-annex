{- Git configuration
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config where

import Common.Annex
import qualified Git
import qualified Git.Config
import qualified Git.Command
import qualified Annex
import Config.Cost
import Types.Availability

type UnqualifiedConfigKey = String
data ConfigKey = ConfigKey String

instance Show ConfigKey where
	show (ConfigKey s) = s

{- Looks up a setting in git config. -}
getConfig :: ConfigKey -> String -> Annex String
getConfig (ConfigKey key) def = fromRepo $ Git.Config.get key def

getConfigMaybe :: ConfigKey -> Annex (Maybe String)
getConfigMaybe (ConfigKey key) = fromRepo $ Git.Config.getMaybe key

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: ConfigKey -> String -> Annex ()
setConfig (ConfigKey key) value = do
	inRepo $ Git.Command.run [Param "config", Param key, Param value]
	Annex.changeGitRepo =<< inRepo Git.Config.reRead

{- Unsets a git config setting. (Leaves it in state currently.) -}
unsetConfig :: ConfigKey -> Annex ()
unsetConfig ck@(ConfigKey key) = ifM (isJust <$> getConfigMaybe ck)
	( inRepo $ Git.Command.run
		[Param "config", Param "--unset", Param key]
	, noop -- avoid unsetting something not set; that would fail
	)

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
remoteCost :: RemoteGitConfig -> Cost -> Annex Cost
remoteCost c def = fromMaybe def <$> remoteCost' c

remoteCost' :: RemoteGitConfig -> Annex (Maybe Cost)
remoteCost' c = case remoteAnnexCostCommand c of
	Just cmd | not (null cmd) -> liftIO $
		readish <$> readProcess "sh" ["-c", cmd]
	_ -> return $ remoteAnnexCost c

setRemoteCost :: Git.Repo -> Cost -> Annex ()
setRemoteCost r c = setConfig (remoteConfig r "cost") (show c)

setRemoteAvailability :: Git.Repo -> Availability -> Annex ()
setRemoteAvailability r c = setConfig (remoteConfig r "availability") (show c)

getNumCopies :: Maybe Int -> Annex Int
getNumCopies (Just v) = return v
getNumCopies Nothing = deprecatedNumCopies

deprecatedNumCopies :: Annex Int
deprecatedNumCopies = fromMaybe 1 . annexNumCopies <$> Annex.getGitConfig

isDirect :: Annex Bool
isDirect = annexDirect <$> Annex.getGitConfig

crippledFileSystem :: Annex Bool
crippledFileSystem = annexCrippledFileSystem <$> Annex.getGitConfig

setCrippledFileSystem :: Bool -> Annex ()
setCrippledFileSystem b = do
	setConfig (annexConfig "crippledfilesystem") (Git.Config.boolConfig b)
	Annex.changeGitConfig $ \c -> c { annexCrippledFileSystem = b }

{- Gets the http headers to use. -}
getHttpHeaders :: Annex [String]
getHttpHeaders = do
	v <- annexHttpHeadersCommand <$> Annex.getGitConfig
	case v of
		Just cmd -> lines <$> liftIO (readProcess "sh" ["-c", cmd])
		Nothing -> annexHttpHeaders <$> Annex.getGitConfig
