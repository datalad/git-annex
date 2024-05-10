{- git-remote-annex program
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module CmdLine.GitRemoteAnnex where

import Annex.Common
import qualified Annex
import qualified Remote
import qualified Git.CurrentRepo
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.Bundle
import qualified Git.Remote
import qualified Git.Remote.Remove
import qualified Annex.SpecialRemote as SpecialRemote
import qualified Annex.Branch
import qualified Types.Remote as Remote
import Annex.Transfer
import Backend.GitRemoteAnnex
import Config
import Types.RemoteConfig
import Types.ProposedAccepted
import Types.Key
import Types.GitConfig
import Git.Types
import Logs.Difference
import Annex.Init
import Annex.Content
import Annex.Perms
import Remote.List
import Remote.List.Util
import Utility.Tmp
import Utility.Env
import Utility.Metered

import Network.URI
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M
import System.FilePath.ByteString as P
import qualified Utility.RawFilePath as R

run :: [String] -> IO ()
run (remotename:url:[]) =
	-- git strips the "annex::" prefix of the url
	-- when running this command, so add it back
	let url' = "annex::" ++ url
	in case parseSpecialRemoteNameUrl remotename url' of
		Left e -> giveup e
		Right src -> do
			repo <- getRepo
			state <- Annex.new repo
			Annex.eval state (run' src)
run (_remotename:[]) = giveup "remote url not configured"
run _ = giveup "expected remote name and url parameters"

run' :: SpecialRemoteConfig -> Annex ()
run' src = do
	sab <- startAnnexBranch
	-- Prevent any usual git-annex output to stdout, because
	-- the output of this command is being parsed by git.
	doQuietAction $
		withSpecialRemote src sab $ \rmt -> do
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
	manifest <- downloadManifest rmt
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
		updateTrackingRefs rmt trackingrefmap

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
	manifest <- maybe (downloadManifest rmt) pure (manifestCache st)
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
	updateTrackingRefs rmt trackingrefs
	(ok, st') <- if M.null trackingrefs
		then pushEmpty st rmt
		else if any forcedPush refspecs
			then fullPush st rmt (M.keys trackingrefs)
			-- TODO: support max-bundles config
			else incrementalPush st rmt
				(trackingRefs st) trackingrefs
	if ok
		then do
			sendresponses responses
			return (ls', st' { trackingRefs = trackingrefs })
		else do
			-- Restore the old tracking refs 
			updateTrackingRefs rmt (trackingRefs st)
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
			Just srcref
				| forcedPush r -> okresp $
					M.insert tr srcref trackingrefs
				| otherwise -> ifM (isfastforward srcref tr)
					( okresp $
						M.insert tr srcref trackingrefs
					, errresp "non-fast-forward"
					)
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
-- All git bundle objects listed in the old manifest will be 
-- deleted after successful upload of the new git bundle and manifest. 
--
-- If this is interrupted, or loses access to the remote mid way through, it
-- will leave the remote with unused bundle keys on it, but every bundle 
-- key listed in the manifest will exist, so it's in a consistent, usable
-- state.
--
-- However, the manifest is replaced by first dropping the object and then
-- uploading a new one. Interrupting that will leave the remote without a
-- manifest, which will appear as if all tracking branches were deleted
-- from it.
fullPush :: State -> Remote -> [Ref] -> Annex (Bool, State)
fullPush st rmt refs = guardPush st $ do
	oldmanifest <- maybe (downloadManifest rmt) pure (manifestCache st)
	let bs = map Git.Bundle.fullBundleSpec refs
	bundlekey <- generateAndUploadGitBundle rmt bs oldmanifest
	uploadManifest rmt (Manifest [bundlekey])
	ok <- allM (dropKey rmt) $
		filter (/= bundlekey) (inManifest oldmanifest)
	return (ok, st { manifestCache = Nothing })

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
	bs <- calc [] (M.toList newtrackingrefs)
	oldmanifest <- maybe (downloadManifest rmt) pure (manifestCache st)
	bundlekey <- generateAndUploadGitBundle rmt bs oldmanifest
	uploadManifest rmt (Manifest [bundlekey])
	return (True, st { manifestCache = Nothing })
  where
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

-- When the push deletes all refs from the remote, upload an empty
-- manifest and then drop all bundles that were listed in it. 
-- The manifest is emptired first so if this is interrupted, only
-- unused bundles will remain in the remote, rather than leaving the
-- remote with a manifest that refers to missing bundles.
pushEmpty :: State -> Remote -> Annex (Bool, State)
pushEmpty st rmt = do
	manifest <- maybe (downloadManifest rmt) pure (manifestCache st)
	uploadManifest rmt (Manifest [])
	ok <- allM (dropKey rmt) 
		(genManifestKey (Remote.uuid rmt) : inManifest manifest)
	return (ok, st { manifestCache = Nothing })

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

-- Runs an action with a Remote as specified by the SpecialRemoteConfig.
withSpecialRemote :: SpecialRemoteConfig -> StartAnnexBranch -> (Remote -> Annex a) -> Annex a
withSpecialRemote (ExistingSpecialRemote remotename) _ a =
	getEnabledSpecialRemoteByName remotename >>=
		maybe (giveup $ "There is no special remote named " ++ remotename)
		a
withSpecialRemote cfg@(SpecialRemoteConfig {}) sab a = case specialRemoteName cfg of
	-- The name could be the name of an existing special remote,
	-- if so use it as long as its UUID matches the UUID from the url.
	Just remotename -> getEnabledSpecialRemoteByName remotename >>= \case
		Just rmt
			| Remote.uuid rmt == specialRemoteUUID cfg -> a rmt
			| otherwise -> giveup $ "The uuid in the annex:: url does not match the uuid of the remote named " ++ remotename
		-- When cloning from an annex:: url,
		-- this is used to set up the origin remote.
		Nothing -> (initremote remotename >>= a)
			`finally` cleanupInitialization sab
	Nothing -> inittempremote
		`finally` cleanupInitialization sab
  where
	-- Initialize a new special remote with the provided configuration
	-- and name.
	--
	-- The configuration is not stored in the git-annex branch, because
	-- it's expected that the git repository stored on the special
	-- remote includes its configuration, perhaps under a different
	-- name, and perhaps slightly different (when the annex:: url
	-- omitted some unimportant part of the configuration).
	initremote remotename = do
		let c = M.insert SpecialRemote.nameField (Proposed remotename)
			(specialRemoteConfig cfg)
		t <- either giveup return (SpecialRemote.findType c)
		dummycfg <- liftIO dummyRemoteGitConfig
		(c', _u) <- Remote.setup t Remote.Init (Just (specialRemoteUUID cfg)) 
			Nothing c dummycfg
			`onException` cleanupremote remotename
		setConfig (remoteConfig c' "url") (specialRemoteUrl cfg)
		remotesChanged
		getEnabledSpecialRemoteByName remotename >>= \case
			Just rmt -> case checkSpecialRemoteProblems rmt of
				Nothing -> return rmt
				Just problem -> do
					cleanupremote remotename
					giveup problem
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
		Just rmt -> 
			maybe (return (Just rmt)) giveup
				(checkSpecialRemoteProblems rmt)

-- Avoid using special remotes that are thirdparty populated, because
-- there is no way to push the git repository keys into one.
--
-- XXX Avoid using special remotes that are encrypted by key
-- material stored in the git repository, since that would present a
-- chicken and egg problem when cloning.
checkSpecialRemoteProblems :: Remote -> Maybe String
checkSpecialRemoteProblems rmt
	| Remote.thirdPartyPopulated (Remote.remotetype rmt) =
		Just "Cannot use this thirdparty-populated special remote as a git remote"
	| otherwise = Nothing

-- The manifest contains an ordered list of git bundle keys.
newtype Manifest = Manifest { inManifest :: [Key] }

-- Downloads the Manifest, or if it does not exist, returns an empty
-- Manifest.
--
-- Throws errors if the remote cannot be accessed or the download fails,
-- or if the manifest file cannot be parsed.
--
-- This downloads the manifest to a temporary file, rather than using
-- the usual Annex.Transfer.download. The content of manifests is not
-- stable, and so it needs to re-download it fresh every time.
downloadManifest :: Remote -> Annex Manifest
downloadManifest rmt = ifM (Remote.checkPresent rmt mk)
	( withTmpFile "GITMANIFEST" $ \tmp tmph -> do
		liftIO $ hClose tmph
		_ <- Remote.retrieveKeyFile rmt mk
			(AssociatedFile Nothing) tmp
			nullMeterUpdate Remote.NoVerify
		ks <- map deserializeKey' . B8.lines <$> liftIO (B.readFile tmp)
		Manifest <$> checkvalid [] ks
	, return (Manifest [])
	)
  where
	mk = genManifestKey (Remote.uuid rmt)

	checkvalid c [] = return (reverse c)
	checkvalid c (Just k:ks) = case fromKey keyVariety k of
		GitBundleKey -> checkvalid (k:c) ks
		_ -> giveup $ "Wrong type of key in manifest " ++ serializeKey k
	checkvalid _ (Nothing:_) =
		giveup $ "Error parsing manifest " ++ serializeKey mk

-- Uploads the Manifest to the remote.
--
-- Throws errors if the remote cannot be accessed or the upload fails.
--
-- The manifest key is first dropped from the remote, then the new
-- content is uploaded. This is necessary because the same key is used,
-- and behavior of remotes is undefined when sending a key that is
-- already present on the remote, but with different content.
--
-- Note that if this is interrupted or loses access to the remote part
-- way through, it may leave the remote without a manifest file. That will
-- appear as if all refs have been deleted from the remote.
-- XXX It should be possible to remember when that happened, by writing
-- state to a file before, and then the next time git-remote-annex is run, it
-- could recover from the situation.
uploadManifest :: Remote -> Manifest -> Annex ()
uploadManifest rmt manifest =
	withTmpFile "GITMANIFEST" $ \tmp tmph -> do
		liftIO $ forM_ (inManifest manifest) $ \bundlekey ->
			B8.hPutStrLn tmph (serializeKey' bundlekey)
		liftIO $ hClose tmph
		-- Remove old manifest if present.
		Remote.removeKey rmt mk
		-- storeKey needs the key to be in the annex objects
		-- directory, so put the manifest file there temporarily.
		-- Using linkOrCopy rather than moveAnnex to avoid updating
		-- InodeCache database. Also, works even when the repository
		-- is configured to require only cryptographically secure
		-- keys, which it is not.
		objfile <- calcRepo (gitAnnexLocation mk)
		res <- modifyContentDir objfile $
			linkOrCopy mk (toRawFilePath tmp) objfile Nothing
		unless (isJust res)
			uploadfailed
		-- noRetry because manifest content is not stable
		ok <- upload rmt mk (AssociatedFile Nothing)
			noRetry noNotification
		-- Don't leave the manifest key in the annex objects
		-- directory.
		unlinkAnnex mk
		unless ok
			uploadfailed
  where
	mk = genManifestKey (Remote.uuid rmt)	
	uploadfailed = giveup $ "Failed to upload " ++ serializeKey mk

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
downloadGitBundle rmt k = 
	ifM (download rmt k (AssociatedFile Nothing) stdRetry noNotification)
		( decodeBS <$> calcRepo (gitAnnexLocation k)
		, giveup $ "Failed to download " ++ serializeKey k
		)

-- Uploads a git bundle from the annex objects directory to the remote.
--
-- Throws errors if the upload fails.
--
-- This does not update the location log to indicate that the remote
-- contains the git bundle object.
uploadGitBundle :: Remote -> Key -> Annex ()
uploadGitBundle rmt k =
	unlessM (upload rmt k (AssociatedFile Nothing) stdRetry noNotification) $
		giveup $ "Failed to upload " ++ serializeKey k

-- Generates a git bundle, ingests it into the local objects directory, 
-- and uploads its key to the special remote.
--
-- If the key is already present in the provided manifest, avoids
-- uploading it.
--
-- On failure, an exception is thrown, and nothing is added to the local
-- objects directory.
generateAndUploadGitBundle
	:: Remote
	-> [Git.Bundle.BundleSpec]
	-> Manifest
	-> Annex Key
generateAndUploadGitBundle rmt bs manifest =
	withTmpFile "GITBUNDLE" $ \tmp tmph -> do
		liftIO $ hClose tmph
		inRepo $ Git.Bundle.create tmp bs
		bundlekey <- genGitBundleKey (Remote.uuid rmt)
			(toRawFilePath tmp) nullMeterUpdate
		unless (bundlekey `elem` (inManifest manifest)) $ do
			unlessM (moveAnnex bundlekey (AssociatedFile Nothing) (toRawFilePath tmp)) $
				giveup "Unable to push"
			uploadGitBundle rmt bundlekey
				`onException` unlinkAnnex bundlekey
		return bundlekey

dropKey :: Remote -> Key -> Annex Bool
dropKey rmt k = tryNonAsync (Remote.removeKey rmt k) >>= \case
	Right () -> return True
	Left ex -> do
		liftIO $ hPutStrLn stderr $
			"Failed to drop " 
				++ serializeKey k 
				++ " (" ++ show ex ++ ")"
		return False

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

-- Update the tracking refs to be those in the map, and no others. 
updateTrackingRefs :: Remote -> M.Map Ref Sha -> Annex ()
updateTrackingRefs rmt new = do
	old <- inRepo $ Git.Ref.forEachRef 
		[Param (decodeBS (trackingRefPrefix rmt))]

	-- Delete all tracking refs that are not in the map.
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
	= AnnexBranchExistedAlready Ref
	| AnnexBranchCreatedEmpty Ref

startAnnexBranch :: Annex StartAnnexBranch
startAnnexBranch = ifM (null <$> Annex.Branch.siblingBranches)
	( AnnexBranchCreatedEmpty <$> Annex.Branch.getBranch
	, AnnexBranchExistedAlready <$> Annex.Branch.getBranch
	)

-- This is run after git has used this process to fetch or push from a
-- special remote that was specified using a git-annex url. If the git
-- repository was not initialized for use by git-annex already, it is still
-- not initialized at this point.
--
-- If the git-annex branch did not exist when this command started,
-- the current contents of it were created in passing by this command,
-- which is hard to avoid. But if a git-annex branch is fetched from the
-- special remote and contains Differences, it would not be possible to
-- merge it into the git-annex branch that was created while running this
-- command. To avoid that problem, when the git-annex branch was created
-- at the start of this command, it's deleted.
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
cleanupInitialization :: StartAnnexBranch -> Annex ()
cleanupInitialization sab = do
	case sab of
		AnnexBranchExistedAlready _ -> noop
		AnnexBranchCreatedEmpty _ -> do
			inRepo $ Git.Branch.delete Annex.Branch.fullname
			indexfile <- fromRepo gitAnnexIndex
			liftIO $ removeWhenExistsWith R.removeLink indexfile
	ifM Annex.Branch.hasSibling
		( do
			autoInitialize' (pure True) remoteList
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
