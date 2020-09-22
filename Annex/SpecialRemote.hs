{- git-annex special remote configuration
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.SpecialRemote (
	module Annex.SpecialRemote,
	module Annex.SpecialRemote.Config
) where

import Annex.Common
import Annex.SpecialRemote.Config
import Types.Remote (RemoteConfig, SetupStage(..), typename, setup)
import Types.GitConfig
import Types.ProposedAccepted
import Config
import Remote.List
import Logs.Remote
import Logs.Trust
import qualified Types.Remote as Remote
import Git.Types (RemoteName)

import qualified Data.Map as M
import Data.Ord

{- See if there's an existing special remote with this name.
 -
 - Prefer remotes that are not dead when a name appears multiple times. -}
findExisting :: RemoteName -> Annex (Maybe (UUID, RemoteConfig, Maybe (ConfigFrom UUID)))
findExisting name = do
	t <- trustMap
	headMaybe
		. sortBy (comparing $ \(u, _, _) -> Down $ M.lookup u t)
		. findByRemoteConfig (\c -> lookupName c == Just name)
		<$> Logs.Remote.remoteConfigMap

newConfig
	:: RemoteName
	-> Maybe (Sameas UUID)
	-> RemoteConfig
	-- ^ configuration provided by the user
	-> M.Map UUID RemoteConfig
	-- ^ configuration of other special remotes, to inherit from
	-- when sameas is used
	-> RemoteConfig
newConfig name sameas fromuser m = case sameas of
	Nothing -> M.insert nameField (Proposed name) fromuser
	Just (Sameas u) -> addSameasInherited m $ M.fromList
		[ (sameasNameField, Proposed name)
		, (sameasUUIDField, Proposed (fromUUID u))
		] `M.union` fromuser

specialRemoteMap :: Annex (M.Map UUID RemoteName)
specialRemoteMap = do
	m <- Logs.Remote.remoteConfigMap
	return $ M.fromList $ mapMaybe go (M.toList m)
  where
	go (u, c) = case lookupName c of
		Nothing -> Nothing
		Just n -> Just (u, n)

{- find the remote type -}
findType :: RemoteConfig -> Either String RemoteType
findType config = maybe unspecified (specified . fromProposedAccepted) $
	M.lookup typeField config
  where
	unspecified = Left "Specify the type of remote with type="
	specified s = case filter (findtype s) remoteTypes of
		[] -> Left $ "Unknown remote type " ++ s 
			++ " (pick from: "
			++ intercalate " " (map typename remoteTypes)
			++ ")"
		(t:_) -> Right t
	findtype s i = typename i == s

autoEnable :: Annex ()
autoEnable = do
	remotemap <- M.filter configured <$> remoteConfigMap
	enabled <- getenabledremotes
	forM_ (M.toList remotemap) $ \(cu, c) -> unless (cu `M.member` enabled) $ do
		let u = case findSameasUUID c of
			Just (Sameas u') -> u'
			Nothing -> cu
		case (lookupName c, findType c) of
			(Just name, Right t) -> whenM (canenable u) $ do
				showSideAction $ "Auto enabling special remote " ++ name
				dummycfg <- liftIO dummyRemoteGitConfig
				tryNonAsync (setup t (Enable c) (Just u) Nothing c dummycfg) >>= \case
					Left e -> warning (show e)
					Right (_c, _u) ->
						when (cu /= u) $
							setConfig (remoteAnnexConfig c "config-uuid") (fromUUID cu)
			_ -> return ()
  where
	configured rc = fromMaybe False $
		trueFalseParser' . fromProposedAccepted
			=<< M.lookup autoEnableField rc
	canenable u = (/= DeadTrusted) <$> lookupTrust u
	getenabledremotes = M.fromList
		. map (\r -> (getcu r, r))
		<$> remoteList
	getcu r = fromMaybe
		(Remote.uuid r)
		(remoteAnnexConfigUUID (Remote.gitconfig r))
