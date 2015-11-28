{- git-annex special remote configuration
 -
 - Copyright 2011-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.SpecialRemote where

import Common.Annex
import Remote (remoteTypes, remoteMap)
import Types.Remote (RemoteConfig, RemoteConfigKey, typename, setup)
import Logs.Remote
import Logs.Trust
import qualified Git.Config

import qualified Data.Map as M
import Data.Ord

type RemoteName = String

{- See if there's an existing special remote with this name.
 -
 - Prefer remotes that are not dead when a name appears multiple times. -}
findExisting :: RemoteName -> Annex (Maybe (UUID, RemoteConfig))
findExisting name = do
	t <- trustMap
	matches <- sortBy (comparing $ \(u, _c) -> M.lookup u t)
		. findByName name
		<$> Logs.Remote.readRemoteLog
	return $ headMaybe matches

newConfig :: RemoteName -> RemoteConfig
newConfig = M.singleton nameKey

findByName :: RemoteName ->  M.Map UUID RemoteConfig -> [(UUID, RemoteConfig)]
findByName n = filter (matching . snd) . M.toList
  where
	matching c = case M.lookup nameKey c of
		Nothing -> False
		Just n'
			| n' == n -> True
			| otherwise -> False

specialRemoteMap :: Annex (M.Map UUID RemoteName)
specialRemoteMap = do
	m <- Logs.Remote.readRemoteLog
	return $ M.fromList $ mapMaybe go (M.toList m)
  where
	go (u, c) = case M.lookup nameKey c of
		Nothing -> Nothing
		Just n -> Just (u, n)

{- find the specified remote type -}
findType :: RemoteConfig -> Either String RemoteType
findType config = maybe unspecified specified $ M.lookup typeKey config
  where
	unspecified = Left "Specify the type of remote with type="
	specified s = case filter (findtype s) remoteTypes of
		[] -> Left $ "Unknown remote type " ++ s
		(t:_) -> Right t
	findtype s i = typename i == s

{- The name of a configured remote is stored in its config using this key. -}
nameKey :: RemoteConfigKey
nameKey = "name"

{- The type of a remote is stored in its config using this key. -}
typeKey :: RemoteConfigKey
typeKey = "type"

autoEnableKey :: RemoteConfigKey
autoEnableKey = "autoenable"

autoEnable :: Annex ()
autoEnable = do
	remotemap <- M.filter configured <$> readRemoteLog
	enabled <- remoteMap id
	forM_ (M.toList remotemap) $ \(u, c) -> unless (u `M.member` enabled) $ do
		case (M.lookup nameKey c, findType c) of
			(Just name, Right t) -> whenM (canenable u) $ do
				showSideAction $ "Auto enabling special remote " ++ name
				res <- tryNonAsync $ setup t (Just u) Nothing c
				case res of
					Left e -> warning (show e)
					Right _ -> return ()
			_ -> return ()
  where
	configured rc = fromMaybe False $
		Git.Config.isTrue =<< M.lookup autoEnableKey rc
	canenable u = (/= DeadTrusted) <$> lookupTrust u
