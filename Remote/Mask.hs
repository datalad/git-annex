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
import Utility.Metered
import qualified Remote.Git

import Control.Concurrent.STM
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
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	maskedremote <- mkMaskedRemote rc gc u
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c $ encryptedRemoteCostAdj + semiExpensiveRemoteCost
	let this = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileInOrder = pure True
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalVerifiableKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = False
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
		, availability = pure LocallyAvailable
		, readonly = False
		, appendonly = False
		, untrustworthy = False
		, mkUnavailable = return Nothing
		, getInfo = getInfo =<< getMaskedRemote maskedremote
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
			-- enableremote with remote= overrides the remote
			-- name that was used with initremote.
			Just (Proposed maskremotename) -> do
				r <- findnamed maskremotename
				unless (uuid r == maskremoteuuid) $
					giveup $ "Remote \"" ++ maskremotename ++ "\" does not have the expected uuid (" ++ fromUUID maskremoteuuid ++ ")" 
				setupremote r
			_ -> enableremote remotelist
  where
	setupremote r = do
		let c' = M.insert remoteUUIDField
			(Proposed (fromUUID (uuid r) :: String)) c
		(c'', encsetup) <- encryptionSetup setupstage c' gc
		verifyencryptionok encsetup r
		
		u <- maybe (liftIO genUUID) return mu
		gitConfigSpecialRemote u c'' [ ("mask", name r) ]
		return (c'', u)
		
	maskremoteuuid = fromMaybe NoUUID $ 
		toUUID . fromProposedAccepted
			<$> M.lookup remoteUUIDField c
				
	enableremote remotelist = do
		case filter (\r -> uuid r == maskremoteuuid) remotelist of
			(r:_) -> setupremote r
			[] -> case setupstage of
				Enable _ ->
					missingMaskedRemote maskremoteuuid
				-- When autoenabling, the masked remote may
				-- get autoenabled later, or need to be
				-- manually enabled.
				_ -> do
					(c', _) <- encryptionSetup setupstage c gc
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

newtype MaskedRemote = MaskedRemote { getMaskedRemote :: Annex Remote }

-- findMaskedRemote won't work until the remote list has been populated,
-- so has to be done on the fly rather than at generation time.
-- This caches it for speed.
mkMaskedRemote :: RemoteConfig -> RemoteGitConfig -> UUID -> Annex MaskedRemote
mkMaskedRemote c gc u = do
	v <- liftIO $ newTMVarIO Nothing
	return $ MaskedRemote $ 
		liftIO (atomically (takeTMVar v)) >>= \case
			Just maskedremote -> return maskedremote
			Nothing -> do
				maskedremote <- findMaskedRemote c gc u
				liftIO $ atomically $ putTMVar v (Just maskedremote)
				return maskedremote

findMaskedRemote :: RemoteConfig -> RemoteGitConfig -> UUID -> Annex Remote
findMaskedRemote c gc myuuid = case remoteAnnexMask gc of
	-- This remote was autoenabled, so use any remote with the
	-- uuid of the masked remote, so that it can also be autoenabled.
	Just "true" -> 
		case getmaskedremoteuuid of
			Just maskremoteuuid -> 
				selectremote maskremoteuuid $ \r ->
					uuid r == maskremoteuuid
			Nothing -> missingMaskedRemote NoUUID
	Just maskremotename ->
		selectremote (fromMaybe NoUUID getmaskedremoteuuid) $ \r -> 
			name r == maskremotename 
				&& Just (uuid r) == getmaskedremoteuuid
	Nothing -> missingMaskedRemote NoUUID
  where
	getmaskedremoteuuid = toUUID . fromProposedAccepted
		<$> M.lookup remoteUUIDField c
	selectremote u f = do
		remotelist <- Annex.getState Annex.remotes
		case filter f remotelist of
			(r:_)
				| uuid r == myuuid -> giveup "Mask special remote is configured to mask itself. This is not a valid configuration."
				-- Avoid cycles, and there is no benefit
				-- to masking a mask special remote.
				| remotetype r == remote -> giveup "Mask special remote is configured to mask another mask special remote. This is not supported."
				| otherwise -> return r
			[] -> missingMaskedRemote u

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

store :: MaskedRemote -> Storer
store maskedremote k src p = do
	r <- getMaskedRemote maskedremote 
	storeMasked r k src p

storeMasked :: Remote -> Storer
storeMasked maskedremote = 
	fileStorer $ \k f p -> storeKey maskedremote k af (Just f) p
  where
	af = AssociatedFile Nothing

retrieve :: MaskedRemote -> Retriever
retrieve maskedremote k p dest iv callback = do
	r <- getMaskedRemote maskedremote 
	fileRetriever (retrieveMasked r) k p dest iv callback

retrieveMasked :: Remote -> OsPath -> Key -> MeterUpdate -> Annex ()
retrieveMasked maskedremote dest k p = 
	-- The masked remote does not need to verify, because fileRetriever
	-- does its own verification.
	void $ retrieveKeyFile maskedremote k af dest p NoVerify
  where
	af = AssociatedFile Nothing

remove :: MaskedRemote -> Remover
remove maskedremote proof k = do
	r <- getMaskedRemote maskedremote 
	removeMasked r proof k

removeMasked :: Remote -> Remover
removeMasked maskedremote = removeKey maskedremote

checkKey :: MaskedRemote -> CheckPresent
checkKey maskedremote k = do
	r <- getMaskedRemote maskedremote 
	checkKeyMasked r k

checkKeyMasked :: Remote -> CheckPresent
checkKeyMasked maskedremote = checkPresent maskedremote

remoteField :: RemoteConfigField
remoteField = Accepted "remote"

remoteUUIDField :: RemoteConfigField
remoteUUIDField = Accepted "remoteuuid"
