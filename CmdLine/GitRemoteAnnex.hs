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
import qualified Git.CurrentRepo
import qualified Remote
import Annex.UUID
import Types.Remote
import Types.Key
import Network.URI
import Utility.Tmp
import Utility.Metered
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as S

run :: [String] -> IO ()
run (_remotename:url:[]) = case parseSpecialRemoteUrl url of
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
		go rmt =<< lines <$> liftIO getContents
  where
	go rmt (l:ls) =
		let (c, v) = splitLine l
		in case c of
			"capabilities" -> capabilities >> go rmt ls	
			"list" -> case v of
				"" -> list rmt False >> go rmt ls
				"for-push" -> list rmt True >> go rmt ls
				_ -> protocolError l
			"fetch" -> fetch rmt (l:ls) >>= go rmt
			"push" -> push rmt (l:ls) >>= go rmt
			_ -> protocolError l
	go _ [] = return ()

protocolError :: String -> a
protocolError l = giveup $ "gitremote-helpers protocol error at " ++ show l

capabilities :: Annex ()
capabilities = do
	liftIO $ putStrLn "fetch"
	liftIO $ putStrLn "push"
	liftIO $ putStrLn ""
	liftIO $ hFlush stdout

list :: Remote -> Bool -> Annex ()
list rmt forpush = error "TODO list" 

-- Any number of fetch commands can be sent by git, asking for specific
-- things. We fetch everything new at once, so find the end of the fetch
-- commands (which is supposed to be a blank line) before fetching. 
fetch :: Remote -> [String] -> Annex [String]
fetch rmt (l:ls) = case splitLine l of
	("fetch", _) -> fetch rmt ls
	("", _) -> do
		fetch' rmt
		return ls
	_ -> do
		fetch' rmt
		return (l:ls)
fetch rmt [] = do
	fetch' rmt
	return []

fetch' :: Remote -> Annex ()
fetch' rmt = error "TODO fetch"

push :: Remote -> [String] -> Annex [String]
push rmt ls = do
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

data SpecialRemoteConfig = SpecialRemoteConfig
	{ specialRemoteUUID :: UUID
	, specialRemoteParams :: [(String, String)]
	}
	deriving (Show)

-- The url for a special remote looks like
-- annex:uuid?param=value&param=value...
parseSpecialRemoteUrl :: String -> Either String SpecialRemoteConfig
parseSpecialRemoteUrl s = case parseURI s of
	Nothing -> Left "URL parse failed"
	Just u -> case uriScheme u of
		"annex:" -> case uriPath u of
			"" -> Left "annex: URL did not include a UUID"
			(':':_) -> Left "annex: URL malformed"
			p -> Right $ SpecialRemoteConfig
				{ specialRemoteUUID = toUUID p
				, specialRemoteParams = parsequery u
				}
		_ -> Left "Not an annex: URL"
  where
	parsequery u = map parsekv $ splitc '&' (drop 1 (uriQuery u))
	parsekv kv =
		let (k, sv) = break (== '=') kv
		    v = if null sv then sv else drop 1 sv
		in (unEscapeString k, unEscapeString v)

getSpecialRemote :: SpecialRemoteConfig -> Annex Remote
getSpecialRemote src
	-- annex:uuid with no query string uses an existing remote
	| null (specialRemoteParams src) = 
		Remote.byUUID (specialRemoteUUID src) >>= \case
			Just rmt -> if thirdPartyPopulated (remotetype rmt) 
				then giveup "Cannot use this thirdparty-populated special remote as a git remote"
				else return rmt
			Nothing -> giveup $ "Cannot find an existing special remote with UUID " 
				++ fromUUID (specialRemoteUUID src)
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

-- A key that is used for the manifest of the git repository stored in a
-- special remote with the specified uuid.
manifestKey :: UUID -> Key
manifestKey u = mkKey $ \kd -> kd
	{ keyName = S.toShort (fromUUID u)
	, keyVariety = OtherKey "GITMANIFEST"
	}

-- A key that is used for the git bundle with the specified sha256
-- that is stored in a special remote with the specified uuid.
gitbundleKey :: UUID -> B.ByteString -> Key
gitbundleKey u sha256 = mkKey $ \kd -> kd
	{ keyName = S.toShort (fromUUID u <> "-" <> sha256)
	, keyVariety = OtherKey "GITBUNDLE"
	}

-- The manifest contains an ordered list of git bundle keys.
newtype Manifest = Manifest [Key]

-- Downloads the Manifest, or if it does not exist, returns an empty
-- Manifest.
--
-- Throws errors if the remote cannot be accessed or the download fails,
-- or if the manifest file cannot be parsed.
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
	mk = manifestKey (Remote.uuid rmt)

	checkvalid c [] = return (reverse c)
	checkvalid c (Just k:ks) = case fromKey keyVariety k of
		OtherKey "GITBUNDLE" -> checkvalid (k:c) ks
		_ -> giveup $ "Wrong type of key in manifest " ++ serializeKey k
	checkvalid _ (Nothing:_) =
		giveup $ "Error parsing manifest " ++ serializeKey mk
