{- git-annex special remote configuration
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
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
import Utility.SafeOutput

import qualified Data.Map as M

{- See if there's an existing special remote with this name.
 -
 - Remotes that are not dead come first in the list
 - when a name appears multiple times. -}
findExisting :: RemoteName -> Annex [(UUID, RemoteConfig, Maybe (ConfigFrom UUID))]
findExisting name = do
	(a, b) <- findExisting' name
	return (a++b)

{- Dead remotes with the name are in the second list, all others in the
 - first list. -}
findExisting' :: RemoteName -> Annex ([(UUID, RemoteConfig, Maybe (ConfigFrom UUID))], [(UUID, RemoteConfig, Maybe (ConfigFrom UUID))])
findExisting' name = do
	t <- trustMap
	partition (\(u, _, _) -> M.lookup u t /= Just DeadTrusted)
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
	return $ specialRemoteNameMap m

specialRemoteNameMap :: M.Map UUID RemoteConfig -> M.Map UUID RemoteName
specialRemoteNameMap = M.fromList . mapMaybe go . M.toList
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
	m <- autoEnableable
	enabled <- getenabledremotes
	forM_ (M.toList m) $ \(cu, c) -> unless (cu `M.member` enabled) $ do
		let u = case findSameasUUID c of
			Just (Sameas u') -> u'
			Nothing -> cu
		case (lookupName c, findType c) of
			(Just name, Right t) -> checkcanenable u name $ do
				showSideAction $ UnquotedString $ "Auto enabling special remote " ++ name
				dummycfg <- liftIO dummyRemoteGitConfig
				tryNonAsync (setup t (AutoEnable c) (Just u) Nothing c dummycfg) >>= \case
					Left e -> warning (UnquotedString (show e))
					Right (_c, _u) ->
						when (cu /= u) $
							setConfig (remoteAnnexConfig c "config-uuid") (fromUUID cu)
			_ -> return ()
  where
	getenabledremotes = M.fromList
		. map (\r -> (getcu r, r))
		<$> remoteList
	getcu r = fromMaybe
		(Remote.uuid r)
		(remoteAnnexConfigUUID (Remote.gitconfig r))
	checkcanenable u name cont
		-- Avoid auto-enabling when the name contains a control
		-- character, because git does not avoid displaying control
		-- characters in the name of a remote, and an attacker could
		-- leverage autoenabling it as part of an attack.
		| safeOutput name /= name = return ()
		| otherwise = do
			rs <- remoteList' False
			case filter (\rmt -> Remote.name rmt == name) rs of
				(rmt:_) | Remote.uuid rmt /= u -> warning $ 
					UnquotedString $ "Cannot auto enable special remote " 
						++ name ++ " because there is another remote with the same name."
				_ -> cont

autoEnableable :: Annex (M.Map UUID RemoteConfig)
autoEnableable = do
	tm <- trustMap
	(M.filterWithKey (notdead tm) . M.filter configured)
		<$> remoteConfigMap
  where
	configured c = fromMaybe False $
		trueFalseParser' . fromProposedAccepted
			=<< M.lookup autoEnableField c
	notdead tm cu c = 
		let u = case findSameasUUID c of
			Just (Sameas u') -> u'
			Nothing -> cu
		in lookupTrust' u tm /= DeadTrusted

