{- git repository handling 
 -
 - This is written to be completely independant of git-annex and should be
 - suitable for other uses.
 -
 - Copyright 2010, 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git (
	Repo(..),
	Ref(..),
	Branch,
	Sha,
	Tag,
	repoIsUrl,
	repoIsSsh,
	repoIsHttp,
	repoIsLocalBare,
	repoDescribe,
	repoLocation,
	workTree,
	gitDir,
	configTrue,
	gitCommandLine,
	run,
	runBool,
	pipeRead,
	pipeWrite,
	pipeWriteRead,
	pipeNullSplit,
	pipeNullSplitB,
	attributes,
	reap,
	assertLocal,
) where

import qualified Data.Map as M
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as L
import Network.URI (uriPath, uriScheme)

import Common
import Git.Types

{- User-visible description of a git repo. -}
repoDescribe :: Repo -> String
repoDescribe Repo { remoteName = Just name } = name
repoDescribe Repo { location = Url url } = show url
repoDescribe Repo { location = Dir dir } = dir
repoDescribe Repo { location = Unknown } = "UNKNOWN"

{- Location of the repo, either as a path or url. -}
repoLocation :: Repo -> String
repoLocation Repo { location = Url url } = show url
repoLocation Repo { location = Dir dir } = dir
repoLocation Repo { location = Unknown } = undefined

{- Some code needs to vary between URL and normal repos,
 - or bare and non-bare, these functions help with that. -}
repoIsUrl :: Repo -> Bool
repoIsUrl Repo { location = Url _ } = True
repoIsUrl _ = False

repoIsSsh :: Repo -> Bool
repoIsSsh Repo { location = Url url } 
	| scheme == "ssh:" = True
	-- git treats these the same as ssh
	| scheme == "git+ssh:" = True
	| scheme == "ssh+git:" = True
	| otherwise = False
	where
		scheme = uriScheme url
repoIsSsh _ = False

repoIsHttp :: Repo -> Bool
repoIsHttp Repo { location = Url url } 
	| uriScheme url == "http:" = True
	| uriScheme url == "https:" = True
	| otherwise = False
repoIsHttp _ = False

configAvail ::Repo -> Bool
configAvail Repo { config = c } = c /= M.empty

repoIsLocalBare :: Repo -> Bool
repoIsLocalBare r@(Repo { location = Dir _ }) = configAvail r && configBare r
repoIsLocalBare _ = False

assertLocal :: Repo -> a -> a
assertLocal repo action = 
	if not $ repoIsUrl repo
		then action
		else error $ "acting on non-local git repo " ++  repoDescribe repo ++ 
				" not supported"
configBare :: Repo -> Bool
configBare repo = maybe unknown configTrue $ M.lookup "core.bare" $ config repo
	where
		unknown = error $ "it is not known if git repo " ++
			repoDescribe repo ++
			" is a bare repository; config not read"

{- Path to a repository's gitattributes file. -}
attributes :: Repo -> String
attributes repo
	| configBare repo = workTree repo ++ "/info/.gitattributes"
	| otherwise = workTree repo ++ "/.gitattributes"

{- Path to a repository's .git directory. -}
gitDir :: Repo -> String
gitDir repo
	| configBare repo = workTree repo
	| otherwise = workTree repo </> ".git"

{- Path to a repository's --work-tree, that is, its top.
 -
 - Note that for URL repositories, this is the path on the remote host. -}
workTree :: Repo -> FilePath
workTree Repo { location = Url u } = uriPath u
workTree Repo { location = Dir d } = d
workTree Repo { location = Unknown } = undefined

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: [CommandParam] -> Repo -> [CommandParam]
gitCommandLine params repo@(Repo { location = Dir _ } ) =
	-- force use of specified repo via --git-dir and --work-tree
	[ Param ("--git-dir=" ++ gitDir repo)
	, Param ("--work-tree=" ++ workTree repo)
	] ++ params
gitCommandLine _ repo = assertLocal repo $ error "internal"

{- Runs git in the specified repo. -}
runBool :: String -> [CommandParam] -> Repo -> IO Bool
runBool subcommand params repo = assertLocal repo $
	boolSystem "git" $ gitCommandLine (Param subcommand : params) repo

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: String -> [CommandParam] -> Repo -> IO ()
run subcommand params repo = assertLocal repo $
	runBool subcommand params repo
		>>! error $ "git " ++ show params ++ " failed"

{- Runs a git subcommand and returns its output, lazily. 
 -
 - Note that this leaves the git process running, and so zombies will
 - result unless reap is called.
 -}
pipeRead :: [CommandParam] -> Repo -> IO L.ByteString
pipeRead params repo = assertLocal repo $ do
	(_, h) <- hPipeFrom "git" $ toCommand $ gitCommandLine params repo
	hSetBinaryMode h True
	L.hGetContents h

{- Runs a git subcommand, feeding it input.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWrite :: [CommandParam] -> L.ByteString -> Repo -> IO PipeHandle
pipeWrite params s repo = assertLocal repo $ do
	(p, h) <- hPipeTo "git" (toCommand $ gitCommandLine params repo)
	L.hPut h s
	hClose h
	return p

{- Runs a git subcommand, feeding it input, and returning its output.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWriteRead :: [CommandParam] -> L.ByteString -> Repo -> IO (PipeHandle, L.ByteString)
pipeWriteRead params s repo = assertLocal repo $ do
	(p, from, to) <- hPipeBoth "git" (toCommand $ gitCommandLine params repo)
	hSetBinaryMode from True
	L.hPut to s
	hClose to
	c <- L.hGetContents from
	return (p, c)

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it. -}
pipeNullSplit :: [CommandParam] -> Repo -> IO [String]
pipeNullSplit params repo = map L.unpack <$> pipeNullSplitB params repo

{- For when Strings are not needed. -}
pipeNullSplitB ::[CommandParam] -> Repo -> IO [L.ByteString]
pipeNullSplitB params repo = filter (not . L.null) . L.split '\0' <$>
	pipeRead params repo

{- Reaps any zombie git processes. -}
reap :: IO ()
reap = do
	-- throws an exception when there are no child processes
	r <- catchDefaultIO (getAnyProcessStatus False True) Nothing
	maybe (return ()) (const reap) r

{- Checks if a string from git config is a true value. -}
configTrue :: String -> Bool
configTrue s = map toLower s == "true"
