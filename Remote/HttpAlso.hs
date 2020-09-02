{- HttpAlso remote (readonly).
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.HttpAlso (remote) where

import Annex.Common
import Types.Remote
import Types.ProposedAccepted
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import Remote.Helper.Special
import qualified Git
import Annex.Content
import Config.Cost
import Config
import Logs.Web
import Creds
import Utility.Metered
import qualified Annex.Url as Url
import Annex.SpecialRemote.Config

import qualified Data.Map as M
import System.FilePath.Posix as P
import Control.Concurrent.STM

remote :: RemoteType
remote = RemoteType
	{ typename = "httpalso"
	, enumerate = const (findSpecialRemotes "httpalso")
	, generate = gen
	, configParser = mkRemoteConfigParser 
		[ optionalStringParser urlField
			(FieldDesc "(required) url to the remote content")
		]
	, setup = httpAlsoSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	}

urlField :: RemoteConfigField
urlField = Accepted "url"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc expensiveRemoteCost
	let url = getRemoteConfigValue urlField c
	ll <- liftIO newLearnedLayout
	return $ Just $ this url ll c cst
  where
	this url ll c cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = uploadKey
		, retrieveKeyFile = downloadKey url ll
		, retrieveKeyFileCheap = Nothing
		-- HttpManagerRestricted is used here, so this is
		-- secure.
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey
		, lockContent = Nothing
		, checkPresent = checkKey url ll (this url ll c cst)
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, getRepo = return r
		, readonly = True
		, appendonly = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

httpAlsoSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
httpAlsoSetup _ Nothing _ _ _ =
	error "Must use --sameas when initializing a httpalso remote."
httpAlsoSetup _ (Just u) _ c gc = do
	_url <- maybe (giveup "Specify url=")
		(return . fromProposedAccepted)
		(M.lookup urlField c)
	(c', _encsetup) <- encryptionSetup c gc
	gitConfigSpecialRemote u c' [("httpalso", "true")]
	return (c', u)

downloadKey :: Maybe URLString -> LearnedLayout -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Verification
downloadKey baseurl ll key _af dest p = do
	unlessM (urlAction baseurl ll key go) $
		giveup "download failed"
	return UnVerified
  where
	go url = Url.withUrlOptions $ downloadUrl key p [url] dest

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex ()
uploadKey _ _ _ = giveup "upload to httpalso special remote not supported"

dropKey :: Key -> Annex ()
dropKey _ = giveup "removal from httpalso special remote not supported"

checkKey :: Maybe URLString -> LearnedLayout -> Remote -> Key -> Annex Bool
checkKey baseurl ll r key = do
	showChecking r
	urlAction baseurl ll key $ \url -> 
		Url.withUrlOptions $ Url.checkBoth url (fromKey keySize key)

type LearnedLayout = TVar (Maybe [Key -> URLString])

newLearnedLayout :: IO LearnedLayout
newLearnedLayout = newTVarIO Nothing

-- Learns which layout the special remote uses, so the once any
-- action on an url succeeds, subsequent calls will continue to use that
-- layout (or related layouts).
urlAction :: Maybe URLString -> LearnedLayout -> Key -> (URLString -> Annex Bool) -> Annex Bool
urlAction (Just baseurl) ll key a = liftIO (readTVarIO ll) >>= \case
	Just learned -> go False [learned]
	Nothing -> go True (supportedLayouts baseurl)
  where
	go _learn [] = return False
	go learn (layouts:rest) = go' learn layouts [] <||> go learn rest
	
	go' _ [] _ = return False
	go' learn (layout:rest) prevs = 
		ifM (a (layout key))
			( do
				when learn $ do
					let learned = layout:prevs++rest
					liftIO $ atomically $
						writeTVar ll (Just learned)
				return True
			, go' learn rest (layout:prevs)
			)
			
-- cannot normally happen
urlAction Nothing _ _ _ = giveup "no url configured for httpalso special remote"

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
	mkurl k hasher = baseurl P.</> fromRawFilePath (hasher k) P.</> kf k
	kf k = fromRawFilePath (keyFile k)
