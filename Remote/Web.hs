{- Web remote.
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Web (remote, getWebUrls) where

import Annex.Common
import Types.Remote
import Types.ProposedAccepted
import Types.Creds
import Remote.Helper.Special
import Remote.Helper.ExportImport
import qualified Git
import qualified Git.Construct
import Annex.Content
import Annex.Verify
import Config.Cost
import Config
import Logs.Web
import Annex.UUID
import Utility.Metered
import Utility.Glob
import qualified Annex.Url as Url
import Annex.YoutubeDl
import Annex.SpecialRemote.Config
import Logs.Remote

import qualified Data.Map as M

remote :: RemoteType
remote = RemoteType
	{ typename = "web"
	, enumerate = list
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser urlincludeField
			(FieldDesc "only use urls matching this glob")
		, optionalStringParser urlexcludeField
			(FieldDesc "don't use urls that match this glob")
		]
	, setup = setupInstance
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

-- The web remote always exists.
-- (If the web should cease to exist, remove this module and redistribute
-- a new release to the survivors by carrier pigeon.)
--
-- There may also be other instances of the web remote, which can be
-- limited to accessing particular urls, and have different costs.
list :: Bool -> Annex [Git.Repo]
list _autoinit = do
	r <- liftIO $ Git.Construct.remoteNamed "web" (pure Git.Construct.fromUnknown)
	others <- findSpecialRemotes "web"
	-- List the main one last, this makes its name be used instead
	-- of the other names when git-annex is referring to content on the
	-- web.
	return (others++[r])

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c expensiveRemoteCost
 	urlincludeexclude <- mkUrlIncludeExclude c
	return $ Just Remote
		{ uuid = if u == NoUUID then webUUID else u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = uploadKey
		, retrieveKeyFile = downloadKey urlincludeexclude
		, retrieveKeyFileCheap = Nothing
		-- HttpManagerRestricted is used here, so this is
		-- secure.
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey urlincludeexclude
		, lockContent = Nothing
		, checkPresent = checkKey urlincludeexclude
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
		, untrustworthy = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		-- claimingUrl makes the web special remote claim
		-- urls that are not claimed by other remotes,
		-- so no need to claim anything here.
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

setupInstance :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
setupInstance _ mu _ c _ = do
	u <- maybe (liftIO genUUID) return mu
	gitConfigSpecialRemote u c [("web", "true")]
	return (c, u)

downloadKey :: UrlIncludeExclude -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfig -> Annex Verification
downloadKey urlincludeexclude key _af dest p vc = 
	go =<< getWebUrls' urlincludeexclude key
  where
	go [] = giveup "no known url"
	go urls = dl (partition (not . isyoutube) (map getDownloader urls)) >>= \case
		Just v -> return v
		Nothing -> giveup $ unwords
			[ "downloading from all"
			, show (length urls)
			, "known url(s) failed"
			]

	dl ([], ytus) = flip getM (map fst ytus) $ \u ->
		ifM (youtubeDlTo key u dest p)
			( return (Just UnVerified)
			, return Nothing
			)
	dl (us, ytus) = do
		iv <- startVerifyKeyContentIncrementally vc key
		ifM (Url.withUrlOptions $ downloadUrl True key p iv (map fst us) dest)
			( finishVerifyKeyContentIncrementally iv >>= \case
				(True, v) -> return (Just v)
				(False, _) -> dl ([], ytus)
			, dl ([], ytus)
			)

	isyoutube (_, YoutubeDownloader) = True
	isyoutube _ = False

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex ()
uploadKey _ _ _ = giveup "upload to web not supported"

dropKey :: UrlIncludeExclude -> Key -> Annex ()
dropKey urlincludeexclude k = mapM_ (setUrlMissing k) =<< getWebUrls' urlincludeexclude k

checkKey :: UrlIncludeExclude -> Key -> Annex Bool
checkKey urlincludeexclude key = do
	us <- getWebUrls' urlincludeexclude key
	if null us
		then return False
		else either giveup return =<< checkKey' key us
checkKey' :: Key -> [URLString] -> Annex (Either String Bool)
checkKey' key us = firsthit us (Right False) $ \u -> do
	let (u', downloader) = getDownloader u
	case downloader of
		YoutubeDownloader -> youtubeDlCheck u'
		_ -> catchMsgIO $
			Url.withUrlOptions $ Url.checkBoth u' (fromKey keySize key)
  where
	firsthit [] miss _ = return miss
	firsthit (u:rest) _ a = do
		r <- a u
		case r of
			Right True -> return r
			_ -> firsthit rest r a

getWebUrls :: Key -> Annex [URLString]
getWebUrls key = getWebUrls' alwaysInclude key

getWebUrls' :: UrlIncludeExclude -> Key -> Annex [URLString]
getWebUrls' urlincludeexclude key = 
	filter supported <$> getUrls key
  where
	supported u = supporteddownloader u 
		&& checkUrlIncludeExclude urlincludeexclude u
	supporteddownloader u = snd (getDownloader u) 
		`elem` [WebDownloader, YoutubeDownloader]

urlincludeField :: RemoteConfigField
urlincludeField = Accepted "urlinclude"

urlexcludeField :: RemoteConfigField
urlexcludeField = Accepted "urlexclude"

data UrlIncludeExclude = UrlIncludeExclude
	{ checkUrlIncludeExclude :: URLString -> Bool
	}

alwaysInclude :: UrlIncludeExclude
alwaysInclude = UrlIncludeExclude { checkUrlIncludeExclude = const True }

mkUrlIncludeExclude :: ParsedRemoteConfig -> Annex UrlIncludeExclude
mkUrlIncludeExclude = go fallback
  where
	go b pc = case (getglob urlincludeField pc, getglob urlexcludeField pc) of
		(Nothing, Nothing) -> b
		(minclude, mexclude) -> mk minclude mexclude

	getglob f pc = do
		glob <- getRemoteConfigValue f pc
		Just $ compileGlob glob CaseInsensitive (GlobFilePath False)
	
	mk minclude mexclude = pure $ UrlIncludeExclude
			{ checkUrlIncludeExclude = \u -> and
				[ case minclude of
					Just glob -> matchGlob glob u
					Nothing -> True
				, case mexclude of
					Nothing -> True
					Just glob -> not (matchGlob glob u)
				]
			}

	-- When nothing to include or exclude is specified, only include
	-- urls that are not explicitly included by other web special remotes.
	fallback = do
		rcs <- M.elems . M.filter iswebremote <$> remoteConfigMap
		l <- forM rcs $ \rc ->
			parsedRemoteConfig remote rc
				>>= go (pure neverinclude)
		pure $ UrlIncludeExclude
			{ checkUrlIncludeExclude = \u ->
				not (any (\c -> checkUrlIncludeExclude c u) l)
			}
	
	iswebremote rc = (fromProposedAccepted <$> M.lookup typeField rc)
		== Just (typename remote)

	neverinclude = UrlIncludeExclude { checkUrlIncludeExclude = const False }
