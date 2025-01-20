{- git-remote-annex program
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module CmdLine.GitRemoteAnnex where

import Annex.Common
import Types.GitRemoteAnnex
import qualified Annex
import qualified Remote
import qualified Git
import qualified Git.CurrentRepo
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.Bundle
import qualified Git.Remote
import qualified Git.Remote.Remove
import qualified Git.Version
import qualified Annex.SpecialRemote as SpecialRemote
import qualified Annex.Branch
import qualified Annex.BranchState
import qualified Annex.Url as Url
import qualified Types.Remote as Remote
import qualified Logs.Remote
import qualified Remote.External
import Remote.Helper.Encryptable (parseEncryptionMethod)
import Annex.Transfer
import Annex.Startup
import Backend.GitRemoteAnnex
import Config
import Types.Key
import Types.RemoteConfig
import Types.ProposedAccepted
import Types.Export
import Types.GitConfig
import Types.BranchState
import Types.Difference
import Types.Crypto
import Git.Types
import Logs.File
import Logs.Difference
import Annex.Init
import Annex.UUID
import Annex.Content
import Annex.Perms
import Annex.Tmp
import Annex.SpecialRemote.Config
import Remote.List
import Remote.List.Util
import Utility.Tmp
import Utility.Tmp.Dir
import Utility.Env
import Utility.Metered
import Utility.FileMode
import qualified Utility.RawFilePath as R

import Network.URI
import Data.Either
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import qualified System.FilePath.ByteString as P
import qualified Data.Set as S

run :: [String] -> IO ()
run (remotename:url:[]) = do
	unlessM Git.Bundle.versionSupported $
		giveup "git-remote-annex needs a newer version of git"
	repo <- getRepo
	state <- Annex.new repo
	Annex.eval state $
		resolveSpecialRemoteWebUrl url >>= \case
			-- git strips the "annex::" prefix of the url
			-- when running this command, so add it back
			Nothing -> parseurl ("annex::" ++ url) pure
			Just url' -> parseurl url' checkAllowedFromSpecialRemoteWebUrl
  where
	parseurl u checkallowed =
		case parseSpecialRemoteNameUrl remotename u of
			Right src -> checkallowed src >>= run' u
			Left e -> giveup e
run (remotename:[]) = giveup $ "remote url not configured for " ++ remotename
run _ = giveup "expected remote name and url parameters"

run' :: String -> SpecialRemoteConfig -> Annex ()
run' url src = do
	sab <- startAnnexBranch
	whenM (Annex.getRead Annex.debugenabled) $
		enableDebugOutput
	-- Prevent any usual git-annex output to stdout, because
	-- the output of this command is being parsed by git.
	doQuietAction $
		withSpecialRemote src sab $ \rmt -> do
			reportFullUrl url rmt
			ls <- lines <$> liftIO getContents
			go rmt ls emptyState
  where
	go rmt (l:ls) st =
		let (c, v) = splitLine l
		in case c of
			"capabilities" -> capabilities >> go rmt ls st
			"list" -> case v of
				"" -> list st rmt False >>= go rmt ls
				"for-push" -> list st rmt True >>= go rmt ls
				_ -> protocolError l
			"fetch" -> fetch st rmt (l:ls)
				>>= \ls' -> go rmt ls' st
			"push" -> push st rmt (l:ls)
				>>= \(ls', st') -> go rmt ls' st'
			"" -> return ()
			_ -> protocolError l
	go _ [] _ = return ()

data State = State
	{ manifestCache :: Maybe Manifest
	, trackingRefs :: M.Map Ref Sha
	}

emptyState :: State
emptyState = State
	{ manifestCache = Nothing
	, trackingRefs = mempty
	}

protocolError :: String -> a
protocolError l = giveup $ "gitremote-helpers protocol error at " ++ show l

capabilities :: Annex ()
capabilities = do
	liftIO $ putStrLn "fetch"
	liftIO $ putStrLn "push"
	liftIO $ putStrLn ""
	liftIO $ hFlush stdout

list :: State -> Remote -> Bool -> Annex State
list st rmt forpush = do
	manifest <- if forpush
		then downloadManifestWhenPresent rmt
		else downloadManifestOrFail rmt
	l <- forM (inManifest manifest) $ \k -> do
		b <- downloadGitBundle rmt k
		heads <- inRepo $ Git.Bundle.listHeads b	
		-- Get all the objects from the bundle. This is done here
		-- so that the tracking refs can be updated with what is
		-- listed, and so what when a full repush is done, all
		-- objects are available to be pushed.
		when forpush $
			inRepo $ Git.Bundle.unbundle b
		-- The bundle may contain tracking refs, or regular refs,
		-- make sure we're operating on regular refs.
		return $ map (\(s, r) -> (fromTrackingRef rmt r, s)) heads
	
	-- Later refs replace earlier refs with the same name.
	let refmap = M.fromList $ concat l
	let reflist = M.toList refmap
	let trackingrefmap = M.mapKeys (toTrackingRef rmt) refmap

	-- When listing for a push, update the tracking refs to match what
	-- was listed. This is necessary in order for a full repush to know
	-- what to push.
	when forpush $
		updateTrackingRefs True rmt trackingrefmap

	-- Respond to git with a list of refs.
	liftIO $ do
		forM_ reflist $ \(ref, sha) ->
			B8.putStrLn $ fromRef' sha <> " " <> fromRef' ref
		-- Newline terminates list of refs.
		putStrLn ""
		hFlush stdout

	-- Remember the tracking refs and manifest.
	return $ st
		{ manifestCache = Just manifest
		, trackingRefs = trackingrefmap
		}

-- Any number of fetch commands can be sent by git, asking for specific
-- things. We fetch everything new at once, so find the end of the fetch
-- commands (which is supposed to be a blank line) before fetching. 
fetch :: State -> Remote -> [String] -> Annex [String]
fetch st rmt (l:ls) = case splitLine l of
	("fetch", _) -> fetch st rmt ls
	("", _) -> do
		fetch' st rmt
		return ls
	_ -> do
		fetch' st rmt
		return (l:ls)
fetch st rmt [] = do
	fetch' st rmt
	return []

fetch' :: State -> Remote -> Annex ()
fetch' st rmt = do
	manifest <- maybe (downloadManifestOrFail rmt) pure (manifestCache st)
	forM_ (inManifest manifest) $ \k ->
		downloadGitBundle rmt k >>= inRepo . Git.Bundle.unbundle
	-- Newline indicates end of fetch.
	liftIO $ do
		putStrLn ""
		hFlush stdout

-- Note that the git bundles that are generated to push contain 
-- tracking refs, rather than the actual refs that the user requested to
-- push. This is done because git bundle does not allow creating a bundle
-- that contains refs with different names than the ones in the git
-- repository. Consider eg, git push remote foo:bar, where the destination
-- ref is bar, but there may be no bar ref locally, or the bar ref may
-- be different than foo. If git bundle supported GIT_NAMESPACE, it would
-- be possible to generate a bundle that contains the specified refs.
push :: State -> Remote -> [String] -> Annex ([String], State)
push st rmt ls = do
	let (refspecs, ls') = collectRefSpecs ls
	(responses, trackingrefs) <- calc refspecs ([], trackingRefs st)
	updateTrackingRefs False rmt trackingrefs
	(ok, st') <- if M.null trackingrefs
		then pushEmpty st rmt
		else if any forcedPush refspecs
			then fullPush st rmt (M.keys trackingrefs)
			else incrementalPush st rmt
				(trackingRefs st) trackingrefs
	if ok
		then do
			sendresponses responses
			return (ls', st' { trackingRefs = trackingrefs })
		else do
			-- Restore the old tracking refs 
			updateTrackingRefs True rmt (trackingRefs st)
			sendresponses $
				map (const "error push failed") refspecs
			return (ls', st')
  where
	calc
		:: [RefSpec]
		-> ([B.ByteString], M.Map Ref Sha)
		-> Annex ([B.ByteString], M.Map Ref Sha)
	calc [] (responses, trackingrefs) = 
		return (reverse responses, trackingrefs)
	calc (r:rs) (responses, trackingrefs) =
		let tr = toTrackingRef rmt (dstRef r)
		    okresp m = pure 
		    	( ("ok " <> fromRef' (dstRef r)):responses
			, m
			)
		    errresp msg = pure
		    	( ("error " <> fromRef' (dstRef r) <> " " <> msg):responses
			, trackingrefs
			)
		in calc rs =<< case srcRef r of
			Just srcref -> inRepo (Git.Ref.sha srcref) >>= \case
				Just sha
					| forcedPush r -> okresp $
						M.insert tr sha trackingrefs
					| otherwise -> ifM (isfastforward sha tr)
						( okresp $
							M.insert tr sha trackingrefs
						, errresp "non-fast-forward"
						)
				Nothing -> errresp "unknown ref"
			Nothing -> okresp $ M.delete tr trackingrefs
	
	-- Check if the push is a fast-forward that will not overwrite work
	-- in the ref currently stored in the remote. This seems redundant
	-- to git's own checking for non-fast-forwards. But unfortunately,
	-- before git push checks that, it actually tells us to push.
	-- That seems likely to be a bug in git, and this is a workaround.
	isfastforward newref tr = case M.lookup tr (trackingRefs st) of
		Just prevsha -> inRepo $ Git.Ref.isAncestor prevsha newref
		Nothing -> pure True
	
	-- Send responses followed by newline to indicate end of push.
	sendresponses responses = liftIO $ do
		mapM_ B8.putStrLn responses
		putStrLn ""
		hFlush stdout

-- Full push of the specified refs to the remote.
fullPush :: State -> Remote -> [Ref] -> Annex (Bool, State)
fullPush st rmt refs = guardPush st $ do
	oldmanifest <- maybe (downloadManifestWhenPresent rmt) pure
		(manifestCache st)
	fullPush' oldmanifest st rmt refs

fullPush' :: Manifest -> State -> Remote -> [Ref] -> Annex (Bool, State)
fullPush' oldmanifest st rmt refs = do
	let bs = map Git.Bundle.fullBundleSpec refs
	(bundlekey, uploadbundle) <- generateGitBundle rmt bs oldmanifest
	let manifest = mkManifest [bundlekey] $
		S.fromList (inManifest oldmanifest)
			`S.union`
		outManifest oldmanifest
	manifest' <- startPush rmt manifest
	uploadbundle
	uploadManifest rmt manifest'
	return (True, st { manifestCache = Nothing })

guardPush :: State -> Annex (Bool, State) -> Annex (Bool, State)
guardPush st a = catchNonAsync a $ \ex -> do
	liftIO $ hPutStrLn stderr $
		"Push failed (" ++ show ex ++ ")"
	return (False, st { manifestCache = Nothing })

-- Incremental push of only the refs that changed.
--
-- No refs were deleted (that causes a fullPush), but new refs may
-- have been added.
incrementalPush :: State -> Remote -> M.Map Ref Sha -> M.Map Ref Sha -> Annex (Bool, State)
incrementalPush st rmt oldtrackingrefs newtrackingrefs = guardPush st $ do
	oldmanifest <- maybe (downloadManifestWhenPresent rmt) pure (manifestCache st)
	if length (inManifest oldmanifest) + 1 > remoteAnnexMaxGitBundles (Remote.gitconfig rmt)
		then fullPush' oldmanifest st rmt (M.keys newtrackingrefs)
		else go oldmanifest
  where
	go oldmanifest = do
		bs <- calc [] (M.toList newtrackingrefs)
		(bundlekey, uploadbundle) <- generateGitBundle rmt bs oldmanifest
		let manifest = oldmanifest <> mkManifest [bundlekey] mempty
		manifest' <- startPush rmt manifest
		uploadbundle
		uploadManifest rmt manifest'
		return (True, st { manifestCache = Nothing })
	
	calc c [] = return (reverse c)
	calc c ((ref, sha):refs) = case M.lookup ref oldtrackingrefs of
		Just oldsha
			| oldsha == sha -> calc c refs -- unchanged
			| otherwise ->
				ifM (inRepo $ Git.Ref.isAncestor oldsha ref)
					( use $ checkprereq oldsha ref
					, use $ findotherprereq ref sha
					)
		Nothing -> use $ findotherprereq ref sha
	  where
		use a = do
			bs <- a
			calc (bs:c) refs
	
	-- Unfortunately, git bundle will let a prerequisite specified
	-- for one ref prevent it including another ref. For example,
	-- where x is a ref that points at A, and y is a ref that points at
	-- B (which has A as its parent), git bundle x A..y
	-- will omit including the x ref in the bundle at all.
	--
	-- But we need to include all (changed) refs that the user
	-- specified to push in the bundle. So, only include the sha
	-- as a prerequisite when it will not prevent including another
	-- changed ref in the bundle.
	checkprereq prereq ref =
		ifM (anyM shadows $ M.elems $ M.delete ref changedrefs)
			( pure $ Git.Bundle.fullBundleSpec ref
			, pure $ Git.Bundle.BundleSpec
				{ Git.Bundle.preRequisiteRef = Just prereq
				, Git.Bundle.includeRef = ref
				}
			)
	  where
		shadows s
			| s == prereq = pure True
			| otherwise = inRepo $ Git.Ref.isAncestor s prereq
		changedrefs = M.differenceWith
			(\a b -> if a == b then Nothing else Just a)
			newtrackingrefs oldtrackingrefs
	
	-- When the old tracking ref is not able to be used as a
	-- prerequisite, this to find some other ref that was previously
	-- pushed that can be used as a prerequisite instead. This can
	-- optimise the bundle size a bit in edge cases.
	--
	-- For example, a forced push of branch foo that resets it back
	-- several commits can use a previously pushed bar as a prerequisite
	-- if it's an ancestor of foo.
	findotherprereq ref sha = 
		findotherprereq' ref sha (M.elems oldtrackingrefs)
	findotherprereq' ref _ [] = pure (Git.Bundle.fullBundleSpec ref)
	findotherprereq' ref sha (l:ls)
		| l == sha = findotherprereq' ref sha ls
		| otherwise = ifM (inRepo $ Git.Ref.isAncestor l ref)
			( checkprereq l ref	
			, findotherprereq' ref sha ls
			)

pushEmpty :: State -> Remote -> Annex (Bool, State)
pushEmpty st rmt = guardPush st $ do
	oldmanifest <- maybe (downloadManifestWhenPresent rmt) pure
		(manifestCache st)
	let manifest = mkManifest mempty $
		S.fromList (inManifest oldmanifest)
			`S.union`
		outManifest oldmanifest
	(manifest', manifestwriter) <- startPush' rmt manifest
	manifest'' <- dropOldKeys rmt manifest'
	manifestwriter manifest''
	uploadManifest rmt manifest''
	return (True, st { manifestCache = Nothing })

data RefSpec = RefSpec
	{ forcedPush :: Bool
	, srcRef :: Maybe Ref -- ^ Nothing when deleting a ref
	, dstRef :: Ref
	}
	deriving (Show)

-- Any number of push commands can be sent by git, specifying the refspecs
-- to push. They should be followed by a blank line.
collectRefSpecs :: [String] -> ([RefSpec], [String])
collectRefSpecs = go []
  where
	go c (l:ls) = case splitLine l of
		("push", refspec) -> go (parseRefSpec refspec:c) ls
		("", _) -> (c, ls)
		_ -> (c, (l:ls))
	go c [] = (c, [])

parseRefSpec :: String -> RefSpec
parseRefSpec ('+':s) = (parseRefSpec s) { forcedPush = True }
parseRefSpec s = 
	let (src, cdst) = break (== ':') s
	    dst = if null cdst then cdst else drop 1 cdst
	    deletesrc = null src
	in RefSpec
		-- To delete a ref, have to do a force push of all
		-- remaining refs.
		{ forcedPush = deletesrc
		, srcRef = if deletesrc
			then Nothing
			else Just (Ref (encodeBS src))
		, dstRef = Ref (encodeBS dst)
		}

-- "foo bar" to ("foo", "bar")
-- "foo" to ("foo", "")
splitLine :: String -> (String, String)
splitLine l = 
	let (c, sv) = break (== ' ') l
	    v = if null sv then sv else drop 1 sv
	in (c, v)

data SpecialRemoteConfig
	= SpecialRemoteConfig
		{ specialRemoteUUID :: UUID
		, specialRemoteConfig :: RemoteConfig
		, specialRemoteName :: Maybe RemoteName
		, specialRemoteUrl :: String
		}
	| ExistingSpecialRemote RemoteName
	deriving (Show)

-- The url for a special remote looks like
-- "annex::uuid?param=value&param=value..."
--
-- Also accept an url of "annex::", when a remote name is provided,
-- to use an already enabled special remote.
parseSpecialRemoteNameUrl :: String -> String -> Either String SpecialRemoteConfig
parseSpecialRemoteNameUrl remotename url
	| url == "annex::" && remotename /= url = Right $
		ExistingSpecialRemote remotename
	| "annex::" `isPrefixOf` remotename = parseSpecialRemoteUrl url Nothing
	| otherwise = parseSpecialRemoteUrl url (Just remotename)

parseSpecialRemoteUrl :: String -> Maybe RemoteName -> Either String SpecialRemoteConfig
parseSpecialRemoteUrl url remotename = case parseURI url of
	Nothing -> Left "URL parse failed"
	Just u -> case uriScheme u of
		"annex:" -> case uriPath u of
			"" -> Left "annex: URL did not include a UUID"
			(':':p)
				| null p -> Left "annex: URL did not include a UUID"
				| otherwise -> Right $ SpecialRemoteConfig
					{ specialRemoteUUID = toUUID p
					, specialRemoteConfig = parsequery u
					, specialRemoteName = remotename
					, specialRemoteUrl = url
				}
			_ -> Left "annex: URL malformed"
		_ -> Left "Not an annex: URL"
  where
	parsequery u = M.fromList $ 
		map parsekv $ splitc '&' (drop 1 (uriQuery u))
	parsekv kv =
		let (k, sv) = break (== '=') kv
		    v = if null sv then sv else drop 1 sv
		in (Proposed (unEscapeString k), Proposed (unEscapeString v))

-- Handles an url that contains a http address, by downloading
-- the web page and using it as the full annex:: url.
-- The passed url has already had "annex::" stripped off.
resolveSpecialRemoteWebUrl :: String -> Annex (Maybe String)
resolveSpecialRemoteWebUrl url
	| "http://" `isPrefixOf` lcurl || "https://" `isPrefixOf` lcurl =
		Url.withUrlOptionsPromptingCreds $ \uo ->
			withTmpFile "git-remote-annex" $ \tmp h -> do
				liftIO $ hClose h
				Url.download' nullMeterUpdate Nothing url tmp uo >>= \case
					Left err -> giveup $ url ++ " " ++ err
					Right () -> liftIO $
						(headMaybe . lines)
							<$> readFileStrict tmp
	| otherwise = return Nothing
  where
	lcurl = map toLower url

-- Only some types of special remotes are allowed to come from
-- resolveSpecialRemoteWebUrl. Throws an error if this one is not.
checkAllowedFromSpecialRemoteWebUrl :: SpecialRemoteConfig -> Annex SpecialRemoteConfig
checkAllowedFromSpecialRemoteWebUrl src@(ExistingSpecialRemote {}) = pure src
checkAllowedFromSpecialRemoteWebUrl src@(SpecialRemoteConfig {}) =
	case M.lookup typeField (specialRemoteConfig src) of
		Nothing -> giveup "Web URL did not include a type field."
		Just t
			| t == Proposed "httpalso" -> return src
			| otherwise -> giveup "Web URL can only be used for a httpalso special remote."

getSpecialRemoteUrl :: Remote -> Annex (Maybe String)
getSpecialRemoteUrl rmt = do
	rcp <- Remote.configParser (Remote.remotetype rmt)
		(unparsedRemoteConfig (Remote.config rmt))
	return $ genSpecialRemoteUrl rmt rcp

genSpecialRemoteUrl :: Remote -> RemoteConfigParser -> Maybe String
genSpecialRemoteUrl rmt rcp
	-- Fields that are accepted by remoteConfigRestPassthrough
	-- are not necessary to include in the url, except perhaps for
	-- external special remotes. If an external special remote sets
	-- some such fields, cannot generate an url.
	| Remote.typename (Remote.remotetype rmt) == Remote.typename Remote.External.remote
		&& any (`notElem` knownfields) (M.keys c) = Nothing
	| otherwise = Just $ 
		"annex::" ++ fromUUID (Remote.uuid rmt) ++ "?" ++
			intercalate "&" (map configpair cs)
  where
	configpair (k, v) = conv k ++ "=" ++ conv v
	conv = escapeURIString isUnescapedInURIComponent
		. fromProposedAccepted
	
	cs = M.toList (M.filterWithKey (\k _ -> k `elem` safefields) c)
		++ case remoteAnnexConfigUUID (Remote.gitconfig rmt) of
			Nothing -> []
			Just cu -> [(Accepted "config-uuid", Accepted (fromUUID cu))]

	c = unparsedRemoteConfig $ Remote.config rmt
	
	-- Hidden fields are used for internal stuff like ciphers
	-- that should not be included in the url.
	safefields = map parserForField $ 
		filter (\p -> fieldDesc p /= HiddenField) ps

	knownfields = map parserForField ps

	ps = SpecialRemote.essentialFieldParsers
		++ remoteConfigFieldParsers rcp

reportFullUrl :: String -> Remote -> Annex ()
reportFullUrl url rmt = 
	when (url == "annex::") $
		getSpecialRemoteUrl rmt >>= \case
			Nothing -> noop
			Just fullurl -> 
				liftIO $ hPutStrLn stderr $ 
					"Full remote url: " ++ fullurl

-- Runs an action with a Remote as specified by the SpecialRemoteConfig.
withSpecialRemote :: SpecialRemoteConfig -> StartAnnexBranch -> (Remote -> Annex a) -> Annex a
withSpecialRemote (ExistingSpecialRemote remotename) sab a =
	getEnabledSpecialRemoteByName remotename >>=
		maybe (giveup $ "There is no special remote named " ++ remotename)
		(specialRemoteFromUrl sab . a)
withSpecialRemote cfg@(SpecialRemoteConfig {}) sab a = case specialRemoteName cfg of
	-- The name could be the name of an existing special remote,
	-- if so use it as long as its UUID matches the UUID from the url.
	Just remotename -> getEnabledSpecialRemoteByName remotename >>= \case
		Just rmt
			| Remote.uuid rmt == specialRemoteUUID cfg -> 
				specialRemoteFromUrl sab (a rmt)
			| otherwise -> giveup $ "The uuid in the annex:: url does not match the uuid of the remote named " ++ remotename
		-- When cloning from an annex:: url,
		-- this is used to set up the origin remote.
		Nothing -> specialRemoteFromUrl sab 
			(initremote remotename >>= a)
	Nothing -> specialRemoteFromUrl sab inittempremote
  where
	-- Initialize a new special remote with the provided configuration
	-- and name. This actually does a Remote.Enable, because the
	-- special remote has already been initialized somewhere before.
	initremote remotename = do
		let c = M.insert SpecialRemote.nameField (Proposed remotename) $
			M.delete (Accepted "config-uuid") $
			specialRemoteConfig cfg
		t <- either giveup return (SpecialRemote.findType c)
		dummycfg <- liftIO dummyRemoteGitConfig
		(c', u) <- Remote.setup t (Remote.Enable c) (Just (specialRemoteUUID cfg)) 
			Nothing c dummycfg
			`onException` cleanupremote remotename
		Logs.Remote.configSet u c'
		setConfig (remoteConfig c' "url") (specialRemoteUrl cfg)
		case M.lookup (Accepted "config-uuid") (specialRemoteConfig cfg) of
			Just cu -> do
				setConfig (remoteAnnexConfig c' "config-uuid")
					(fromProposedAccepted cu)
				-- This is not quite the same as what is
				-- usually stored to the git-annex branch
				-- for the config-uuid, but it will work.
				-- This change will never be committed to the
				-- git-annex branch.
				Logs.Remote.configSet (toUUID (fromProposedAccepted cu)) c'
			Nothing -> noop
		remotesChanged
		getEnabledSpecialRemoteByName remotename >>= \case
			Just rmt -> return rmt
			Nothing -> do
				cleanupremote remotename
				giveup "Unable to find special remote after setup."

	-- Temporarily initialize a special remote, and remove it after
	-- the action is run.
	inittempremote = 
		let remotename = Git.Remote.makeLegalName $
			"annex-temp-" ++ fromUUID (specialRemoteUUID cfg)
		in bracket
			(initremote remotename)
			(const $ cleanupremote remotename)
			a
	
	cleanupremote remotename = do
		l <- inRepo Git.Remote.listRemotes
		when (remotename `elem` l) $
			inRepo $ Git.Remote.Remove.remove remotename

-- When a special remote has already been enabled, just use it.
getEnabledSpecialRemoteByName :: RemoteName -> Annex (Maybe Remote)
getEnabledSpecialRemoteByName remotename = 
	Remote.byNameOnly remotename >>= \case
		Nothing -> return Nothing
		Just rmt
			-- If the git-annex branch is missing or does not
			-- have a remote config for this remote, but the
			-- git config has the remote, it can't be used.
			| unparsedRemoteConfig (Remote.config rmt) == mempty ->
				return Nothing
			| otherwise -> 
				maybe (Just <$> importTreeWorkAround rmt) giveup
					(checkSpecialRemoteProblems rmt)

checkSpecialRemoteProblems :: Remote -> Maybe String
checkSpecialRemoteProblems rmt
	-- Avoid using special remotes that are thirdparty populated, 
	-- because there is no way to push the git repository keys into one.
	| Remote.thirdPartyPopulated (Remote.remotetype rmt) =
		Just $ "Cannot use this thirdparty-populated special"
			++ " remote as a git remote."
	| parseEncryptionMethod (unparsedRemoteConfig (Remote.config rmt)) /= Right NoneEncryption
		&& not (remoteAnnexAllowEncryptedGitRepo (Remote.gitconfig rmt)) =
			Just $ "Using an encrypted special remote as a git"
				++ " remote makes it impossible to clone"
				++ " from it. If you will never need to"
				++ " clone from this remote, set: git config "
				++ decodeBS allowencryptedgitrepo ++ " true"
	| otherwise = Nothing
  where
	ConfigKey allowencryptedgitrepo = remoteAnnexConfig rmt "allow-encrypted-gitrepo"

-- Using importTree remotes needs the content identifier database to be
-- populated, but it is not when cloning, and cannot be updated when
-- pushing since git-annex branch updates by this program are prevented.
--
-- So, generate instead a version of the remote that uses exportTree actions,
-- which do not need content identifiers. Since Remote.Helper.exportImport
-- replaces the exportActions in exportActionsForImport with ones that use
-- import actions, have to instantiate a new remote with a modified config.
importTreeWorkAround :: Remote -> Annex Remote
importTreeWorkAround rmt
	| not (importTree (Remote.config rmt)) = pure rmt
	| not (exportTree (Remote.config rmt)) = giveup "Using special remotes with importtree=yes but without exporttree=yes as git remotes is not supported."
	| otherwise = do
		m <- Logs.Remote.remoteConfigMap
		r <- Remote.getRepo rmt
		remoteGen' adjustconfig m (Remote.remotetype rmt) r >>= \case
			Just rmt' -> return rmt'
			Nothing -> giveup "Failed to use importtree=yes remote."
  where
	adjustconfig = M.delete importTreeField

-- Downloads the Manifest when present in the remote. When not present,
-- returns an empty Manifest.
downloadManifestWhenPresent :: Remote -> Annex Manifest
downloadManifestWhenPresent rmt = fromMaybe mempty <$> downloadManifest rmt

-- Downloads the Manifest, or fails if the remote does not contain it.
downloadManifestOrFail :: Remote -> Annex Manifest
downloadManifestOrFail rmt =
	maybe (giveup "No git repository found in this remote.") return
		=<< downloadManifest rmt

-- Downloads the Manifest or Nothing if the remote does not contain a
-- manifest.
--
-- Throws errors if the remote cannot be accessed or the download fails,
-- or if the manifest file cannot be parsed.
downloadManifest :: Remote -> Annex (Maybe Manifest)
downloadManifest rmt = get mkmain >>= maybe (get mkbak) (pure . Just)
  where
	mkmain = genManifestKey (Remote.uuid rmt)
	mkbak = genBackupManifestKey (Remote.uuid rmt)

	get mk = getKeyExportLocations rmt mk >>= \case
		Nothing -> ifM (Remote.checkPresent rmt mk)
			( gettotmp $ \tmp ->
				Remote.retrieveKeyFile rmt mk
					(AssociatedFile Nothing) tmp
					nullMeterUpdate Remote.NoVerify
			, return Nothing
			)
		Just locs -> getexport mk locs

	-- Downloads to a temporary file, rather than using eg
	-- Annex.Transfer.download that would put it in the object
	-- directory. The content of manifests is not stable, and so
	-- it needs to re-download it fresh every time, and the object
	-- file should not be stored locally.
	gettotmp dl = withOtherTmp $ \othertmp ->
		withTmpFileIn (fromRawFilePath othertmp) "GITMANIFEST" $ \tmp tmph -> do
			liftIO $ hClose tmph
			_ <- dl tmp
			b <- liftIO (B.readFile tmp)
			case parseManifest b of
				Right m -> Just <$> verifyManifest rmt m
				Left err -> giveup err

	getexport _ [] = return Nothing
	getexport mk (loc:locs) =
		ifM (Remote.checkPresentExport (Remote.exportActions rmt) mk loc)
			( gettotmp $ \tmp -> 
				Remote.retrieveExport (Remote.exportActions rmt)
					mk loc tmp nullMeterUpdate
			, getexport mk locs
			)

-- Uploads the Manifest to the remote.
--
-- Throws errors if the remote cannot be accessed or the upload fails.
--
-- The manifest key is first dropped from the remote, then the new
-- content is uploaded. This is necessary because the same key is used,
-- and behavior of remotes is undefined when sending a key that is
-- already present on the remote, but with different content.
--
-- So this may be interrupted and leave the manifest key not present.
-- To deal with that, there is a backup manifest key. This takes care
-- to ensure that one of the two keys will always exist.
uploadManifest :: Remote -> Manifest -> Annex ()
uploadManifest rmt manifest = do
	ok <- ifM (Remote.checkPresent rmt mkbak)
		( dropandput mkmain <&&> dropandput mkbak
		-- The backup manifest doesn't exist, so upload
		-- it first, and then the manifest second.
		-- This ensures that at no point are both deleted.
		, put mkbak <&&> dropandput mkmain
		)
	unless ok
		uploadfailed
  where
	mkmain = genManifestKey (Remote.uuid rmt)
	mkbak = genBackupManifestKey (Remote.uuid rmt)
	
	uploadfailed = giveup "Failed to upload manifest."

	dropandput mk = do
		dropKey' rmt mk
		put mk

	put mk = withTmpFile "GITMANIFEST" $ \tmp tmph -> do
		liftIO $ B8.hPut tmph (formatManifest manifest)
		liftIO $ hClose tmph
		-- Uploading needs the key to be in the annex objects
		-- directory, so put the manifest file there temporarily.
		-- Using linkOrCopy rather than moveAnnex to avoid updating
		-- InodeCache database. Also, works even when the repository
		-- is configured to require only cryptographically secure
		-- keys, which it is not.
		objfile <- calcRepo (gitAnnexLocation mk)
		modifyContentDir objfile $
			linkOrCopy mk (toRawFilePath tmp) objfile Nothing >>= \case
				-- Important to set the right perms even
				-- though the object is only present
				-- briefly, since sending objects may rely
				-- on or even copy file perms.
				Just _ -> do
					liftIO $ R.setFileMode objfile
						=<< defaultFileMode
					freezeContent objfile
				Nothing -> uploadfailed
		ok <- (uploadGitObject rmt mk >> pure True)
			`catchNonAsync` (const (pure False))
		-- Don't leave the manifest key in the annex objects
		-- directory.
		unlinkAnnex mk
		return ok

formatManifest :: Manifest -> B.ByteString
formatManifest manifest =
	B8.unlines $ 
		map serializeKey' (inManifest manifest)
			<>
		map (\k -> "-" <> serializeKey' k)
			(S.toList (outManifest manifest))

parseManifest :: B.ByteString -> Either String Manifest
parseManifest b = 
	let (outks, inks) = partitionEithers $ map parseline $ B8.lines b
	in case (checkvalid [] inks, checkvalid [] outks) of
		(Right inks', Right outks') -> 
			Right $ mkManifest inks' (S.fromList outks')
		(Left err, _) -> Left err
		(_, Left err) -> Left err
  where
	parseline l
		| "-" `B.isPrefixOf` l = 
			Left $ deserializeKey' $ B.drop 1 l
		| otherwise =
			Right $ deserializeKey' l
	
	checkvalid c [] = Right (reverse c)
	checkvalid c (Just k:ks) = case fromKey keyVariety k of
		GitBundleKey -> checkvalid (k:c) ks
		_ -> Left $ "Wrong type of key in manifest " ++ serializeKey k
	checkvalid _ (Nothing:_) =
		Left "Error parsing manifest"

{- A manifest file is cached here before it or the bundles listed in it
 - is uploaded to the special remote.
 - 
 - This prevents forgetting which bundles were uploaded when a push gets
 - interrupted before updating the manifest on the remote, or when a race
 - causes the uploaded manigest to be overwritten.
 -}
lastPushedManifestFile :: UUID -> Git.Repo -> RawFilePath
lastPushedManifestFile u r = gitAnnexDir r P.</> "git-remote-annex" 
	P.</> fromUUID u P.</> "manifest"

{- Call before uploading anything. The returned manifest has added
 - to it any bundle keys that were in the lastPushedManifestFile
 - and that are not in the new manifest. -}
startPush :: Remote -> Manifest -> Annex Manifest
startPush rmt manifest = do
	(manifest', writer) <- startPush' rmt manifest
	writer manifest'
	return manifest'

startPush' :: Remote -> Manifest -> Annex (Manifest, Manifest -> Annex ())
startPush' rmt manifest = do
	f <- fromRepo (lastPushedManifestFile (Remote.uuid rmt))
	oldmanifest <- liftIO $ 
		fromRight mempty . parseManifest
			<$> B.readFile (fromRawFilePath f)
				`catchNonAsync` (const (pure mempty))
	let oldmanifest' = mkManifest [] $
		S.fromList (inManifest oldmanifest)
			`S.union`
		outManifest oldmanifest
	let manifest' = manifest <> oldmanifest'
	let writer = writeLogFile f . decodeBS . formatManifest
	return (manifest', writer)

-- Drops the outManifest keys. Returns a version of the manifest with
-- any outManifest keys that were successfully dropped removed from it.
--
-- If interrupted at this stage, or if a drop fails, the key remains
-- in the outManifest, so the drop will be tried again later.
dropOldKeys :: Remote -> Manifest -> Annex Manifest
dropOldKeys rmt manifest =
	mkManifest (inManifest manifest) . S.fromList
		<$> filterM (not <$$> dropKey rmt)
			(S.toList (outManifest manifest))

-- When pushEmpty raced with another push, it could result in the manifest
-- listing bundles that it deleted. Such a manifest has to be treated the
-- same as an empty manifest. To detect that, this checks that all the
-- bundles listed in the manifest still exist on the remote.
verifyManifest :: Remote -> Manifest -> Annex Manifest
verifyManifest rmt manifest = 
	ifM (allM (checkPresentGitBundle rmt) (inManifest manifest))
		( return manifest
		, return $ mkManifest [] $
			S.fromList (inManifest manifest)
				`S.union`
			outManifest manifest
		)

-- Downloads a git bundle to the annex objects directory, unless
-- the object file is already present. Returns the filename of the object
-- file.
--
-- Throws errors if the download fails, or the checksum does not verify.
--
-- This does not update the location log to indicate that the local
-- repository contains the git bundle object. Reasons not to include:
-- 1. When this is being used in a git clone, the repository will not have
--    a UUID yet.
-- 2. It would unncessarily bloat the git-annex branch, which would then
--    lead to more things needing to be pushed to the special remote,
--    and so more things pulled from it, etc.
-- 3. Git bundle objects are not usually transferred between repositories
--    except special remotes (although the user can if they want to).
downloadGitBundle :: Remote -> Key -> Annex FilePath
downloadGitBundle rmt k = getKeyExportLocations rmt k >>= \case
	Nothing -> dlwith $ 
		download rmt k (AssociatedFile Nothing) stdRetry noNotification
	Just locs -> dlwith $
		anyM getexport locs
  where
	dlwith a = ifM a
		( decodeBS <$> calcRepo (gitAnnexLocation k)
		, giveup $ "Failed to download " ++ serializeKey k
		)

	getexport loc = catchNonAsync (getexport' loc) (const (pure False))
	getexport' loc =
		getViaTmp rsp vc k (AssociatedFile Nothing) Nothing $ \tmp -> do
			v <- Remote.retrieveExport (Remote.exportActions rmt)
				k loc (decodeBS tmp) nullMeterUpdate
			return (True, v)
	rsp = Remote.retrievalSecurityPolicy rmt
	vc = Remote.RemoteVerify rmt

-- Checks if a bundle is present. Throws errors if the remote cannot be
-- accessed.
checkPresentGitBundle :: Remote -> Key -> Annex Bool
checkPresentGitBundle rmt k = 
	getKeyExportLocations rmt k >>= \case
		Nothing -> Remote.checkPresent rmt k
		Just locs -> anyM checkexport locs
  where
	checkexport = Remote.checkPresentExport (Remote.exportActions rmt) k

-- Uploads a bundle or manifest object from the annex objects directory
-- to the remote.
--
-- Throws errors if the upload fails.
--
-- This does not update the location log to indicate that the remote
-- contains the git object.
uploadGitObject :: Remote -> Key -> Annex ()
uploadGitObject rmt k = getKeyExportLocations rmt k >>= \case
	Just (loc:_) -> do
		objfile <- fromRawFilePath <$> calcRepo (gitAnnexLocation k)
		Remote.storeExport (Remote.exportActions rmt) objfile k loc nullMeterUpdate
	_ -> 
		unlessM (upload rmt k (AssociatedFile Nothing) retry noNotification) $
			giveup $ "Failed to upload " ++ serializeKey k
  where
	retry = case fromKey keyVariety k of
		GitBundleKey -> stdRetry
		-- Manifest keys are not stable
		_ -> noRetry

-- Generates a git bundle, ingests it into the local objects directory, 
-- and returns an action that uploads its key to the special remote.
--
-- If the key is already present in the provided manifest, avoids
-- ingesting or uploading it.
--
-- On failure, an exception is thrown, and nothing is added to the local
-- objects directory.
generateGitBundle
	:: Remote
	-> [Git.Bundle.BundleSpec]
	-> Manifest
	-> Annex (Key, Annex ())
generateGitBundle rmt bs manifest =
	withTmpFile "GITBUNDLE" $ \tmp tmph -> do
		liftIO $ hClose tmph
		inRepo $ Git.Bundle.create tmp bs
		bundlekey <- genGitBundleKey (Remote.uuid rmt)
			(toRawFilePath tmp) nullMeterUpdate
		if (bundlekey `notElem` inManifest manifest)
			then do
				unlessM (moveAnnex bundlekey (AssociatedFile Nothing) (toRawFilePath tmp)) $
					giveup "Unable to push"
				return (bundlekey, uploadaction bundlekey)
			else return (bundlekey, noop)
  where
	uploadaction bundlekey = 
		uploadGitObject rmt bundlekey
			`onException` unlinkAnnex bundlekey

dropKey :: Remote -> Key -> Annex Bool
dropKey rmt k = tryNonAsync (dropKey' rmt k) >>= \case
	Right () -> return True
	Left ex -> do
		liftIO $ hPutStrLn stderr $
			"Failed to drop " 
				++ serializeKey k 
				++ " (" ++ show ex ++ ")"
		return False

dropKey' :: Remote -> Key -> Annex ()
dropKey' rmt k = getKeyExportLocations rmt k >>= \case
	Nothing -> Remote.removeKey rmt Nothing k
	Just locs -> forM_ locs $ \loc -> 
		Remote.removeExport (Remote.exportActions rmt) k loc

getKeyExportLocations :: Remote -> Key -> Annex (Maybe [ExportLocation])
getKeyExportLocations rmt k = do
	cfg <- Annex.getGitConfig
	u <- getUUID
	return $ keyExportLocations rmt k cfg u

-- When the remote contains a tree, the git keys are stored
-- inside the .git/annex/objects/ directory in the remote.
--
-- The first ExportLocation in the returned list is the one that
-- should be used to store a key. But it's possible
-- that one of the others in the list was used.
keyExportLocations :: Remote -> Key -> GitConfig -> UUID -> Maybe [ExportLocation]
keyExportLocations rmt k cfg uuid
	| exportTree (Remote.config rmt) || importTree (Remote.config rmt) = 
		Just $ map (\p -> mkExportLocation (".git" P.</> p)) $
			concatMap (`annexLocationsBare` k) cfgs
	| otherwise = Nothing
  where
	-- When git-annex has not been initialized yet (eg, when cloning), 
	-- the Differences are unknown, so make a version of the GitConfig
	-- with and without the OneLevelObjectHash difference.
	cfgs
		| uuid /= NoUUID = [cfg]
		| hasDifference OneLevelObjectHash (annexDifferences cfg) =
			[ cfg
			, cfg { annexDifferences = mempty }
			]
		| otherwise =
			[ cfg
			, cfg 
				{ annexDifferences = mkDifferences 
					(S.singleton OneLevelObjectHash)
				}
			]

-- Tracking refs are used to remember the refs that are currently on the
-- remote. This is different from git's remote tracking branches, since it
-- needs to track all refs on the remote, not only the refs that the user
-- chooses to fetch.
--
-- For refs/heads/master, the tracking ref is
-- refs/namespaces/git-remote-annex/uuid/refs/heads/master,
-- using the uuid of the remote. See gitnamespaces(7).
trackingRefPrefix :: Remote -> B.ByteString
trackingRefPrefix rmt = "refs/namespaces/git-remote-annex/"
	<> fromUUID (Remote.uuid rmt) <> "/"

toTrackingRef :: Remote -> Ref -> Ref
toTrackingRef rmt (Ref r) = Ref $ trackingRefPrefix rmt <> r

-- If the ref is not a tracking ref, it is returned as-is.
fromTrackingRef :: Remote -> Ref -> Ref
fromTrackingRef rmt = Git.Ref.removeBase (decodeBS (trackingRefPrefix rmt))

-- Update the tracking refs to be those in the map.
-- When deleteold is set, any other tracking refs are deleted.
updateTrackingRefs :: Bool -> Remote -> M.Map Ref Sha -> Annex ()
updateTrackingRefs deleteold rmt new = do
	old <- inRepo $ Git.Ref.forEachRef 
		[Param (decodeBS (trackingRefPrefix rmt))]

	-- Delete all tracking refs that are not in the map.
	when deleteold $
		forM_ (filter (\p -> M.notMember (fst p) new) old) $ \(s, r) ->
			inRepo $ Git.Ref.delete s r
	
	-- Update all changed tracking refs.
	let oldmap = M.fromList (map (\(s, r) -> (r, s)) old)
	forM_ (M.toList new) $ \(r, s) ->
		case M.lookup r oldmap of
			Just s' | s' == s -> noop
			_ -> inRepo $ Git.Branch.update' r s

-- git clone does not bother to set GIT_WORK_TREE when running this
-- program, and it does not run it inside the new git repo either.
-- GIT_DIR is set to the new git directory. So, have to override
-- the worktree to be the parent of the gitdir.
getRepo :: IO Repo
getRepo = getEnv "GIT_WORK_TREE" >>= \case
	Just _ -> Git.CurrentRepo.get
	Nothing -> fixup <$> Git.CurrentRepo.get
  where
	fixup r@(Repo { location = loc@(Local { worktree = Just _ }) }) =
		r { location = loc { worktree = Just (P.takeDirectory (gitdir loc)) } }
	fixup r = r

-- Records what the git-annex branch was at the beginning of this command.
data StartAnnexBranch
	= AnnexBranchExistedAlready Sha
	| AnnexBranchCreatedEmpty Sha

{- Run early in the command, gets the initial state of the git-annex
 - branch.
 -
 - If the branch does not exist yet, it's created here. This is done
 - because it's hard to avoid the branch being created by this command,
 - so tracking the sha of the created branch allows cleaning it up later.
 -}
startAnnexBranch :: Annex StartAnnexBranch
startAnnexBranch = ifM (null <$> Annex.Branch.siblingBranches)
	( AnnexBranchCreatedEmpty <$> Annex.Branch.getBranch
	, AnnexBranchExistedAlready <$> Annex.Branch.getBranch
	)

-- This runs an action that will set up a special remote that
-- was specified using an annex url.
--
-- Setting up a special remote needs to write its config to the git-annex
-- branch. And using a special remote may also write to the branch.
-- But in this case, writes to the git-annex branch need to be avoided,
-- so that cleanupInitialization can leave things in the right state.
--
-- So this prevents commits to the git-annex branch, and redirects all
-- journal writes to a temporary directory, so that all writes
-- to the git-annex branch by the action will be discarded.
specialRemoteFromUrl :: StartAnnexBranch -> Annex a -> Annex a
specialRemoteFromUrl sab a = withTmpDir "journal" $ \tmpdir -> do
	Annex.overrideGitConfig $ \c -> 
		c { annexAlwaysCommit = False }
	Annex.BranchState.changeState $ \st -> 
		st { alternateJournal = Just (toRawFilePath tmpdir) }
	a `finally` cleanupInitialization sab tmpdir

-- If the git-annex branch did not exist when this command started,
-- it was created empty by this command, and this command has avoided
-- making any other commits to it, writing any temporary annex branch
-- changes to the alternateJournal, which can now be discarded. 
-- 
-- If nothing else has written to the branch while this command was running,
-- the branch will be deleted. That allows for the git-annex branch that is
-- fetched from the special remote to contain Differences, which would prevent
-- it from being merged with the git-annex branch created by this command.
--
-- If there is still not a sibling git-annex branch, this deletes all annex
-- objects for git bundles from the annex objects directory, and deletes
-- the annex objects directory. That is necessary to avoid the 
-- Annex.Init.objectDirNotPresent check preventing a later initialization.
-- And if the later initialization includes Differences, the git bundle
-- objects downloaded by this process would be in the wrong locations.
--
-- When there is now a sibling git-annex branch, this handles
-- initialization. When the initialized git-annex branch has Differences,
-- the git bundle objects are in the wrong place, so have to be deleted.
--
-- Unfortunately, git 2.45.1 and related releases added a 
-- "defense in depth" check that a freshly cloned repository
-- does not contain any hooks. Since initialization installs
-- hooks, have to work around that by not initializing, and 
-- delete the git bundle objects.
--
-- Similarly, when on a crippled filesystem, doing initialization would
-- involve checking out an adjusted branch. But git clone wants to do its
-- own checkout. So no initialization is done then, and the git bundle
-- objects are deleted.
cleanupInitialization :: StartAnnexBranch -> FilePath -> Annex ()
cleanupInitialization sab alternatejournaldir = void $ tryNonAsync $ do
	liftIO $ mapM_ R.removeLink
		=<< dirContents (toRawFilePath alternatejournaldir)
	case sab of
		AnnexBranchExistedAlready _ -> noop
		AnnexBranchCreatedEmpty r ->
			whenM ((r ==) <$> Annex.Branch.getBranch) $ do
				indexfile <- fromRepo gitAnnexIndex
				liftIO $ removeWhenExistsWith R.removeLink indexfile
				-- When cloning failed and this is being
				-- run as an exception is thrown, HEAD will
				-- not be set to a valid value, which will
				-- prevent deleting the git-annex branch.
				-- But that's ok, git will delete the 
				-- repository it failed to clone into.
				-- So skip deleting to avoid an ugly
				-- message.
				inRepo Git.Branch.currentUnsafe >>= \case
					Nothing -> return ()
					Just _ -> void $ tryNonAsync $
						inRepo $ Git.Branch.delete Annex.Branch.fullname
	ifM (Annex.Branch.hasSibling <&&> nonbuggygitversion <&&> notcrippledfilesystem)
		( do
			autoInitialize' (pure True) startupAnnex remoteList
			differences <- allDifferences <$> recordedDifferences
			when (differences /= mempty) $
				deletebundleobjects
		, deletebundleobjects
		)
  where
	deletebundleobjects = do
		annexobjectdir <- fromRepo gitAnnexObjectDir
		ks <- listKeys InAnnex
		forM_ ks $ \k -> case fromKey keyVariety k of
                	GitBundleKey -> lockContentForRemoval k noop removeAnnex
			_ -> noop
		void $ liftIO $ tryIO $ removeDirectory (decodeBS annexobjectdir)

	notcrippledfilesystem = not <$> probeCrippledFileSystem

	nonbuggygitversion = liftIO $
		flip notElem buggygitversions <$> Git.Version.installed
	buggygitversions = map Git.Version.normalize
		[ "2.45.1"
		, "2.44.1"
		, "2.43.4"
		, "2.42.2"
		, "2.41.1"
		, "2.40.2"
		, "2.39.4"
		]
