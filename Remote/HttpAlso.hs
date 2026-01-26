{- HttpAlso remote (readonly).
 -
 - Copyright 2020-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.HttpAlso (remote) where

import Annex.Common
import Types.Remote
import Types.ProposedAccepted
import Types.Export
import Remote.Helper.ExportImport
import Remote.Helper.Special
import qualified Git
import Config.Cost
import Config
import Logs.Web
import Creds
import Utility.Metered
import Annex.Verify
import qualified Annex.Url as Url
import Annex.SpecialRemote.Config
import Git.FilePath

import Data.Either
import qualified Data.Map as M
import System.FilePath.Posix as P
import Control.Concurrent.STM

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "httpalso"
	, enumerate = const (findSpecialRemotes "httpalso")
	, generate = gen
	, configParser = mkRemoteConfigParser 
		[ optionalStringParser urlField
			(FieldDesc "(required) url to the remote content")
		]
	, setup = httpAlsoSetup
	, exportSupported = exportIsSupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

urlField :: RemoteConfigField
urlField = Accepted "url"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c expensiveRemoteCost
	let url = getRemoteConfigValue urlField c
	ll <- liftIO newLearnedLayout
	return $ Just $ specialRemote c
		cannotModify
		(downloadKey gc url ll)
		cannotModify
		(checkKey gc url ll)
		(this url c cst)
  where
	this url c cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = cannotModify
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileInOrder = pure True
		, retrieveKeyFileCheap = Nothing
		-- HttpManagerRestricted is used here, so this is
		-- secure.
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = cannotModify
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = False
		, exportActions = ExportActions
			{ storeExport = cannotModify
			, retrieveExport = retriveExportHttpAlso gc url
			, removeExport = cannotModify
			, checkPresentExport = checkPresentExportHttpAlso gc url
			, removeExportDirectory = Nothing
			, renameExport = cannotModify
			}
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairKey = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, getRepo = return r
		, readonly = True
		, appendonly = False
		, untrustworthy = False
		, availability = pure GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

cannotModify :: a
cannotModify = giveup "httpalso special remote is read only"

httpAlsoSetup :: SetupStage -> Maybe UUID -> RemoteName -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
httpAlsoSetup _ Nothing _ _ _ _ =
	giveup "Must use --sameas when initializing a httpalso remote."
httpAlsoSetup ss (Just u) _ _ c gc = do
	_url <- maybe (giveup "Specify url=")
		(return . fromProposedAccepted)
		(M.lookup urlField c)
	c' <- if isJust (M.lookup encryptionField c)
		then fst <$> encryptionSetup ss c gc
		else pure c
	gitConfigSpecialRemote u c' [("httpalso", "true")]
	return (c', u)

downloadKey :: RemoteGitConfig -> Maybe URLString -> LearnedLayout -> Retriever
downloadKey gc baseurl ll = fileRetriever' $ \dest key p iv ->
	downloadAction gc dest p iv (keyUrlAction baseurl ll key)

retriveExportHttpAlso :: RemoteGitConfig -> Maybe URLString -> Key -> ExportLocation -> OsPath -> MeterUpdate -> Annex Verification
retriveExportHttpAlso gc baseurl key loc dest p = do
	verifyKeyContentIncrementally AlwaysVerify key $ \iv ->
		downloadAction gc dest p iv (exportLocationUrlAction baseurl loc)

downloadAction :: RemoteGitConfig -> OsPath -> MeterUpdate -> Maybe IncrementalVerifier -> ((URLString -> Annex (Either String ())) -> Annex (Either String ())) -> Annex ()
downloadAction gc dest p iv run =
	Url.withUrlOptions (Just gc) $ \uo ->
		run (\url -> Url.download' p iv url dest uo)
			>>= either giveup (const (return ()))

checkKey :: RemoteGitConfig -> Maybe URLString -> LearnedLayout -> CheckPresent
checkKey gc baseurl ll key =
	isRight <$> keyUrlAction baseurl ll key (checkKey' gc key)

checkKey' :: RemoteGitConfig -> Key -> URLString -> Annex (Either String ())
checkKey' gc key url = 
	ifM (Url.withUrlOptions (Just gc) $ Url.checkBoth url (fromKey keySize key))
		( return (Right ())
		, return (Left "content not found")
		)

checkPresentExportHttpAlso :: RemoteGitConfig -> Maybe URLString -> Key -> ExportLocation -> Annex Bool
checkPresentExportHttpAlso gc baseurl key loc =
	isRight <$> exportLocationUrlAction baseurl loc (checkKey' gc key)

type LearnedLayout = TVar (Maybe [Key -> URLString])

newLearnedLayout :: IO LearnedLayout
newLearnedLayout = newTVarIO Nothing

-- Learns which layout the special remote uses, so once any action on an
-- url succeeds, subsequent calls will continue to use that layout
-- (or related layouts).
keyUrlAction
	:: Maybe URLString
	-> LearnedLayout
	-> Key
	-> (URLString -> Annex (Either String ()))
	-> Annex (Either String ())
keyUrlAction (Just baseurl) ll key downloader =
	liftIO (readTVarIO ll) >>= \case
		Just learned -> go Nothing False [learned]
		Nothing -> go Nothing True (supportedLayouts baseurl)
  where
	go err learn [] = go' err learn [] []
	go err learn (layouts:rest) = go' err learn layouts [] >>= \case
		Right () -> return (Right ())
		Left err' -> go (Just err') learn rest
	
	go' (Just err) _ [] _ = pure (Left err)
	go' Nothing _ [] _ = error "internal"
	go' _err learn (layout:rest) prevs = 
		downloader (layout key) >>= \case
			Right () -> do
				when learn $ do
					let learned = layout:prevs++rest
					liftIO $ atomically $
						writeTVar ll (Just learned)
				return (Right ())
			Left err -> go' (Just err) learn rest (layout:prevs)
keyUrlAction Nothing _ _ _ = noBaseUrlError

exportLocationUrlAction
	:: Maybe URLString
	-> ExportLocation
	-> (URLString -> Annex (Either String ()))
	-> Annex (Either String ())
exportLocationUrlAction (Just baseurl) loc a =
	a (baseurl P.</> fromOsPath (fromExportLocation loc))
exportLocationUrlAction Nothing _ _ = noBaseUrlError

-- cannot normally happen
noBaseUrlError :: Annex a
noBaseUrlError = giveup "no url configured for httpalso special remote"

-- Different ways that keys can be laid out in the special remote,
-- with the more common first.
--
-- This is a nested list, because a single remote may use more than one
-- layout. In particular, old versions of git-annex used hashDirMixed
-- for some special remotes, before switching to hashDirLower for new data.
-- So, when learning the layout, both need to be tried.
supportedLayouts :: URLString -> [[Key -> URLString]]
supportedLayouts baseurl =
	-- Layout used for bare git-annex repos, and for many
	-- special remotes like directory.
	[ [ \k -> mkurl k (hashDirLower (HashLevels 2)) P.</> kf k
	-- Layout used for non-bare git-annex repos, and for some old
	-- special remotes.
	  , \k -> mkurl k (hashDirMixed (HashLevels 2)) P.</> kf k
	  ]
	-- Special remotes that do not need hash directories.
	, [ \k -> baseurl P.</> kf k ]
	-- Layouts without a key directory, used by some special remotes.
	, [ \k -> mkurl k (hashDirLower def)
	  , \k -> mkurl k (hashDirMixed def)
	  ]
	-- Layouts with only 1 level of hash directory, 
	-- rather than the default 2.
	, [ \k -> mkurl k (hashDirLower (HashLevels 1))
	  , \k -> mkurl k (hashDirMixed (HashLevels 1))
	  ]
	]
  where
	mkurl k hasher = baseurl
		-- On windows, the hasher uses `\` path separators,
		-- but for an url, it needs to use '/'.
		-- So, use toInternalGitPath.
		P.</> fromOsPath (toInternalGitPath (hasher k)) 
		P.</> kf k
	kf k = fromOsPath (keyFile k)
