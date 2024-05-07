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
import Git.Types
import Backend.GitRemoteAnnex
import Annex.Transfer
import Types.Remote
import Types.Key
import Network.URI
import Utility.Tmp
import Utility.Metered
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as M

run :: [String] -> IO ()
run (remotename:url:[]) = 
	-- git strips the "annex::" prefix of the url
	-- when running this command, so add it back
	let url' = "annex::" ++ url
	in case parseSpecialRemoteNameUrl remotename url' of
		Left e -> giveup e
		Right src -> do
			state <- Annex.new =<< Git.CurrentRepo.get
			Annex.eval state (run' src)
run (_remotename:[]) = giveup "remote url not configured"
run _ = giveup "expected remote name and url parameters"

run' :: SpecialRemoteConfig -> Annex ()
run' src =
	-- Prevent any usual git-annex output to stdout, because
	-- the output of this command is being parsed by git.
	doQuietAction $ do
		rmt <- getSpecialRemote src
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
		, specialRemoteParams :: [(String, String)]
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
	| otherwise = parseSpecialRemoteUrl url

parseSpecialRemoteUrl :: String -> Either String SpecialRemoteConfig
parseSpecialRemoteUrl url = case parseURI url of
	Nothing -> Left "URL parse failed"
	Just u -> case uriScheme u of
		"annex:" -> case uriPath u of
			"" -> Left "annex: URL did not include a UUID"
			(':':p) -> Right $ SpecialRemoteConfig
				{ specialRemoteUUID = toUUID p
				, specialRemoteParams = parsequery u
				}
			_ -> Left "annex: URL malformed"
		_ -> Left "Not an annex: URL"
  where
	parsequery u = map parsekv $ splitc '&' (drop 1 (uriQuery u))
	parsekv kv =
		let (k, sv) = break (== '=') kv
		    v = if null sv then sv else drop 1 sv
		in (unEscapeString k, unEscapeString v)

getSpecialRemote :: SpecialRemoteConfig -> Annex Remote
getSpecialRemote (ExistingSpecialRemote remotename) = 
	Remote.byNameOnly remotename >>= \case
		Just rmt -> if thirdPartyPopulated (remotetype rmt) 
			then giveup "Cannot use this thirdparty-populated special remote as a git remote"
			else return rmt
		Nothing -> giveup $ "There is no special remote named " ++ remotename 
getSpecialRemote src@(SpecialRemoteConfig {})
	-- Given the configuration of a special remote, create a
	-- Remote object to access the special remote.
	-- This needs to avoid storing the configuration in the git-annex
	-- branch (which would be redundant and also the configuration
	-- provided may differ in some small way from the configuration
	-- that is stored in the git repository inside the remote, which
	-- should not be changed). It also needs to avoid creating a git
	-- remote in .git/config.
	| otherwise = error "TODO conjure up a new special remote out of thin air"
	-- XXX one way to do it would be to make a temporary git repo,
	-- initremote in there, and use that for accessing the special
	-- remote, rather than the current git repo. But can this be
	-- avoided?

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
downloadManifest rmt = ifM (checkPresent rmt mk)
	( withTmpFile "GITMANIFEST" $ \tmp tmph -> do
		liftIO $ hClose tmph
		_ <- retrieveKeyFile rmt mk
			(AssociatedFile Nothing) tmp
			nullMeterUpdate NoVerify
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
