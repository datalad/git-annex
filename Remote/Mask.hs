{- Mask another remote with added encryption
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.Mask (remote) where

import Annex.Common
import Types.Remote
import Types.Creds
import Types.Crypto
import qualified Git
import qualified Annex
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Config
import Config.Cost
import Annex.UUID
import Types.ProposedAccepted
import Annex.SpecialRemote.Config
import Logs.UUID
import qualified Remote.Git

import qualified Data.Map as M

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "mask"
	, enumerate = const (findSpecialRemotes "mask")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser remoteField
			(FieldDesc "remote to mask")
		]
	, setup = maskSetup
	, exportSupported = exportIsSupported
	, importSupported = importIsSupported
	, thirdPartyPopulated = False
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	maskedremote <- getMaskedRemote rc gc
	let inherited d f = case maskedremote of
		Right mr -> f mr
		Left _ -> d
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c $ encryptedRemoteCostAdj + 
		inherited semiExpensiveRemoteCost cost
	let this = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileInOrder = pure True
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = inherited RetrievalVerifiableKeysSecure retrievalSecurityPolicy
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = inherited False checkPresentCheap
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, getRepo = return r
		, gitconfig = gc
		, localpath = Nothing
		, remotetype = remote
		, availability = inherited (pure Unavailable) availability
		, readonly = inherited False readonly
		, appendonly = inherited False appendonly
		, untrustworthy = inherited False untrustworthy
		, mkUnavailable = return Nothing
		, getInfo = inherited (pure []) getInfo
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}
	return $ Just $ specialRemote c
		(store maskedremote)
		(retrieve maskedremote)
		(remove maskedremote)
		(checkKey maskedremote)
		this

maskSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
maskSetup setupstage mu _ c gc = do
	remotelist <- Annex.getState Annex.remotes
	let findnamed maskremotename =
		case filter (\r -> name r == maskremotename) remotelist of
			(r:_) -> return r
			[] -> giveup $ "There is no remote named \"" ++ maskremotename ++ "\""
	case setupstage of
		Init -> do
			maskremotename <- maybe
				(giveup "Specify remote=")
				(pure . fromProposedAccepted)
				(M.lookup remoteField c)
			setupremote =<< findnamed maskremotename
		_ -> case M.lookup remoteField c of
			Just (Proposed maskremotename) ->
				setupremote =<< findnamed maskremotename
			_ -> enableremote remotelist
  where
	setupremote r = do
		let c' = M.insert remoteUUIDField
			(Proposed (fromUUID (uuid r) :: String)) c
		(c'', encsetup) <- encryptionSetup c' gc
		verifyencryptionok encsetup r
		
		u <- maybe (liftIO genUUID) return mu
		gitConfigSpecialRemote u c'' [ ("mask", name r) ]
		return (c'', u)
				
	enableremote remotelist = do
		let maskremoteuuid = fromMaybe NoUUID $ 
			toUUID . fromProposedAccepted
				<$> M.lookup remoteUUIDField c
		case filter (\r -> uuid r == maskremoteuuid) remotelist of
			(r:_) -> setupremote r
			[] -> case setupstage of
				Enable _ ->
					missingMaskedRemote maskremoteuuid
				-- When autoenabling, the masked remote may
				-- get autoenabled later.
				_ -> do
					(c', _) <- encryptionSetup c gc
					u <- maybe (liftIO genUUID) return mu
					gitConfigSpecialRemote u c' [ ("mask", "true") ]
					return (c', u)

	verifyencryptionok NoEncryption _ =
		giveup "Must use encryption with a mask special remote."
	verifyencryptionok EncryptionIsSetup r
		| remotetype r == Remote.Git.remote =
			verifyencryptionokgit
		| otherwise = noop
	
	verifyencryptionokgit = case parseEncryptionMethod c of
		Right SharedEncryption ->
			giveup "It's not secure to use encryption=shared with a git remote."
		_ -> noop

getMaskedRemote :: RemoteConfig -> RemoteGitConfig -> Annex (Either UUID Remote)
getMaskedRemote c gc = case remoteAnnexMask gc of
	-- This remote was autoenabled, so use any remote with the
	-- uuid of the masked remote, so that it can also be autoenabled.
	Just "true" -> 
		case getmaskedremoteuuid of
			Just maskremoteuuid -> 
				selectremote (\r -> uuid r == maskremoteuuid)
					maskremoteuuid
			Nothing -> return (Left NoUUID)
	Just maskremotename ->
		selectremote (\r -> name r == maskremotename) $
			(fromMaybe NoUUID getmaskedremoteuuid)
	Nothing -> return (Left NoUUID)
  where
	getmaskedremoteuuid = toUUID . fromProposedAccepted <$> M.lookup remoteField c
	selectremote f fallback = do
		remotelist <- Annex.getState Annex.remotes
		case filter f remotelist of
			(r:_) -> return (Right r)
			[] -> return (Left fallback)

missingMaskedRemote :: UUID -> Annex a
missingMaskedRemote maskremoteuuid = do
	descmap <- uuidDescMap
	let desc = case M.lookup maskremoteuuid descmap of
		Just (UUIDDesc d) -> decodeBS d
		Nothing -> ""
	giveup $ unlines
		[ "Before this mask special remote can be used, you must set up the remote it uses:"
		, "  " ++ fromUUID maskremoteuuid ++ " -- " ++ desc
		]

store :: Either UUID Remote -> Storer
store (Right maskedremote) k src p = undefined
store (Left maskedremoteuuid) _ _ _ = missingMaskedRemote maskedremoteuuid

retrieve :: Either UUID Remote -> Retriever
retrieve (Right maskedremote) k p dest iv callback = undefined
retrieve (Left maskedremoteuuid) _ _ _ _ _ = missingMaskedRemote maskedremoteuuid

remove :: Either UUID Remote -> Remover
remove (Right maskedremote) proof k = undefined
remove (Left maskedremoteuuid) _ _ = missingMaskedRemote maskedremoteuuid

checkKey :: Either UUID Remote -> CheckPresent
checkKey (Right maskedremote) k = undefined
checkKey (Left maskedremoteuuid) _ = missingMaskedRemote maskedremoteuuid

remoteField :: RemoteConfigField
remoteField = Accepted "remote"

remoteUUIDField :: RemoteConfigField
remoteUUIDField = Accepted "remoteuuid"
