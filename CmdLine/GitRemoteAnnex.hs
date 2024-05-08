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
import qualified Git
import qualified Git.Ref
import qualified Git.Branch
import qualified Git.Bundle
import qualified Git.Remote
import qualified Git.Remote.Remove
import qualified Annex.SpecialRemote as SpecialRemote
import qualified Annex.Branch
import qualified Types.Remote as R
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
run' src =
	-- Prevent any usual git-annex output to stdout, because
	-- the output of this command is being parsed by git.
	doQuietAction $
		withSpecialRemote src $ \rmt -> do
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
			"fetch" -> fetch st rmt (l:ls) >>= \ls' -> go rmt ls' st
			"push" -> push st rmt (l:ls) >>= \ls' -> go rmt ls' st
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

	-- Remember the tracking refs.
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

push :: State -> Remote -> [String] -> Annex [String]
push st rmt ls = do
	let (refspecs, ls') = collectRefSpecs ls
	error "TODO push refspecs"
	return ls'

data RefSpec = RefSpec
	{ forcedPush :: Bool
	, srcRef :: Maybe String -- empty when deleting a ref
	, dstRef :: String
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
	in RefSpec
		{ forcedPush = False
		, srcRef = if null src then Nothing else Just src
		, dstRef = dst
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
			(':':p) -> Right $ SpecialRemoteConfig
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
withSpecialRemote :: SpecialRemoteConfig -> (Remote -> Annex a) -> Annex a
withSpecialRemote (ExistingSpecialRemote remotename) a =
	getEnabledSpecialRemoteByName remotename >>=
		maybe (giveup $ "There is no special remote named " ++ remotename)
		a
withSpecialRemote cfg@(SpecialRemoteConfig {}) a = case specialRemoteName cfg of
	-- The name could be the name of an existing special remote,
	-- if so use it as long as its UUID matches the UUID from the url.
	Just remotename -> getEnabledSpecialRemoteByName remotename >>= \case
		Just rmt
			| R.uuid rmt == specialRemoteUUID cfg -> a rmt
			| otherwise -> giveup $ "The uuid in the annex:: url does not match the uuid of the remote named " ++ remotename
		-- When cloning from an annex:: url,
		-- this is used to set up the origin remote.
		Nothing -> (initremote remotename >>= a)
			`finally` cleanupInitialization
	Nothing -> inittempremote
		`finally` cleanupInitialization
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
		(c', _u) <- R.setup t R.Init (Just (specialRemoteUUID cfg)) 
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
	| R.thirdPartyPopulated (R.remotetype rmt) =
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
downloadManifest rmt = ifM (R.checkPresent rmt mk)
	( withTmpFile "GITMANIFEST" $ \tmp tmph -> do
		liftIO $ hClose tmph
		_ <- R.retrieveKeyFile rmt mk
			(AssociatedFile Nothing) tmp
			nullMeterUpdate R.NoVerify
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

-- This is run after git has used this process to fetch or push from a
-- special remote that was specified using a git-annex url. If the git
-- repository was not initialized for use by git-annex already, it is still
-- not initialized at this point.
--
-- It's important that initialization not be done by this process until
-- git has fetched any git-annex branch from the special remote. That
-- git-annex branch may have Differences, and prematurely initializing the
-- local repository would then create a git-annex branch that can't merge
-- with the one from the special remote.
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
-- FIXME git-annex branch is unfortunately created during git clone from a
-- special remote. Should not be for this to work.
cleanupInitialization :: Annex ()
cleanupInitialization = ifM Annex.Branch.hasSibling
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
