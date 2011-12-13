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
	Repo,
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
	workTreeFile,
	gitDir,
	urlPath,
	urlHost,
	urlPort,
	urlHostUser,
	urlAuthority,
	urlScheme,
	configMap,
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
	remotes,
	remotesAdd,
	repoRemoteName,
	repoRemoteNameSet,
	repoRemoteNameFromKey,
	checkAttr,
	decodeGitFile,
	encodeGitFile,
	reap,
	useIndex,
	getSha,
	shaSize,
	commit,
	assertLocal,

	prop_idempotent_deencode
) where

import qualified Data.Map as M hiding (map, split)
import Network.URI
import Data.Char
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encode)
import Text.Printf
import System.Exit
import System.Posix.Env (setEnv, unsetEnv, getEnv)
import qualified Data.ByteString.Lazy.Char8 as L

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

{- Constructs and returns an updated version of a repo with
 - different remotes list. -}
remotesAdd :: Repo -> [Repo] -> Repo
remotesAdd repo rs = repo { remotes = rs }

{- Returns the name of the remote that corresponds to the repo, if
 - it is a remote. -}
repoRemoteName :: Repo -> Maybe String
repoRemoteName Repo { remoteName = Just name } = Just name
repoRemoteName _ = Nothing

{- Sets the name of a remote. -}
repoRemoteNameSet :: String -> Repo -> Repo
repoRemoteNameSet n r = r { remoteName = Just n }

{- Sets the name of a remote based on the git config key, such as
   "remote.foo.url". -}
repoRemoteNameFromKey :: String -> Repo -> Repo
repoRemoteNameFromKey k = repoRemoteNameSet basename
	where
		basename = join "." $ reverse $ drop 1 $
				reverse $ drop 1 $ split "." k

{- Some code needs to vary between URL and normal repos,
 - or bare and non-bare, these functions help with that. -}
repoIsUrl :: Repo -> Bool
repoIsUrl Repo { location = Url _ } = True
repoIsUrl _ = False

repoIsSsh :: Repo -> Bool
repoIsSsh Repo { location = Url url } 
	| uriScheme url == "ssh:" = True
	-- git treats these the same as ssh
	| uriScheme url == "git+ssh:" = True
	| uriScheme url == "ssh+git:" = True
	| otherwise = False
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
		else error $ "acting on URL git repo " ++  repoDescribe repo ++ 
				" not supported"
assertUrl :: Repo -> a -> a
assertUrl repo action = 
	if repoIsUrl repo
		then action
		else error $ "acting on local git repo " ++  repoDescribe repo ++ 
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
workTree r@(Repo { location = Url _ }) = urlPath r
workTree (Repo { location = Dir d }) = d
workTree Repo { location = Unknown } = undefined

{- Given a relative or absolute filename inside a git repository's
 - workTree, calculates the name to use to refer to that file to git.
 -
 - This is complicated because the best choice can vary depending on
 - whether the cwd is in a subdirectory of the git repository, or not.
 -
 - For example, when adding a file "/tmp/repo/foo", it's best to refer
 - to it as "foo" if the cwd is outside the repository entirely
 - (this avoids a gotcha with using the full path name when /tmp/repo
 - is itself a symlink). But, if the cwd is "/tmp/repo/subdir",
 - it's best to refer to "../foo".
 -}
workTreeFile :: FilePath -> Repo -> IO FilePath
workTreeFile file repo@(Repo { location = Dir d }) = do
	cwd <- getCurrentDirectory
	let file' = absfile cwd
	unless (inrepo file') $
		error $ file ++ " is not located inside git repository " ++ absrepo
	if inrepo $ addTrailingPathSeparator cwd
		then return $ relPathDirToFile cwd file'
		else return $ drop (length absrepo) file'
	where
		-- normalize both repo and file, so that repo
		-- will be substring of file
		absrepo = maybe bad addTrailingPathSeparator $ absNormPath "/" d
		absfile c = fromMaybe file $ secureAbsNormPath c file
		inrepo f = absrepo `isPrefixOf` f
		bad = error $ "bad repo" ++ repoDescribe repo
workTreeFile _ repo = assertLocal repo $ error "internal"

{- Path of an URL repo. -}
urlPath :: Repo -> String
urlPath Repo { location = Url u } = uriPath u
urlPath repo = assertUrl repo $ error "internal"

{- Scheme of an URL repo. -}
urlScheme :: Repo -> String
urlScheme Repo { location = Url u } = uriScheme u
urlScheme repo = assertUrl repo $ error "internal"

{- Work around a bug in the real uriRegName
 - <http://trac.haskell.org/network/ticket/40> -}
uriRegName' :: URIAuth -> String
uriRegName' a = fixup $ uriRegName a
	where
		fixup x@('[':rest)
			| rest !! len == ']' = take len rest
			| otherwise = x
			where
				len  = length rest - 1
		fixup x = x

{- Hostname of an URL repo. -}
urlHost :: Repo -> String
urlHost = urlAuthPart uriRegName'

{- Port of an URL repo, if it has a nonstandard one. -}
urlPort :: Repo -> Maybe Integer
urlPort r = 
	case urlAuthPart uriPort r of
		":" -> Nothing
		(':':p) -> readMaybe p
		_ -> Nothing

{- Hostname of an URL repo, including any username (ie, "user@host") -}
urlHostUser :: Repo -> String
urlHostUser r = urlAuthPart uriUserInfo r ++ urlAuthPart uriRegName' r

{- The full authority portion an URL repo. (ie, "user@host:port") -}
urlAuthority :: Repo -> String
urlAuthority = urlAuthPart assemble
	where
		assemble a = uriUserInfo a ++ uriRegName' a ++ uriPort a

{- Applies a function to extract part of the uriAuthority of an URL repo. -}
urlAuthPart :: (URIAuth -> a) -> Repo -> a
urlAuthPart a Repo { location = Url u } = a auth
	where
		auth = fromMaybe (error $ "bad url " ++ show u) (uriAuthority u)
urlAuthPart _ repo = assertUrl repo $ error "internal"

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

{- Forces git to use the specified index file.
 - Returns an action that will reset back to the default
 - index file. -}
useIndex :: FilePath -> IO (IO ())
useIndex index = do
	res <- getEnv var
	setEnv var index True
	return $ reset res
	where
		var = "GIT_INDEX_FILE"
		reset (Just v) = setEnv var v True
		reset _ = unsetEnv var

{- Runs an action that causes a git subcommand to emit a sha, and strips
   any trailing newline, returning the sha. -}
getSha :: String -> IO String -> IO Sha
getSha subcommand a = do
	t <- a
	let t' = if last t == '\n'
		then init t
		else t
	when (length t' /= shaSize) $
		error $ "failed to read sha from git " ++ subcommand ++ " (" ++ t' ++ ")"
	return $ Ref t'

{- Size of a git sha. -}
shaSize :: Int
shaSize = 40

{- Commits the index into the specified branch (or other ref), 
 - with the specified parent refs, and returns the committed sha -}
commit :: String -> Branch -> [Ref] -> Repo -> IO Sha
commit message branch parentrefs repo = do
	tree <- getSha "write-tree" $ asString $
		pipeRead [Param "write-tree"] repo
	sha <- getSha "commit-tree" $ asString $
		ignorehandle $ pipeWriteRead
			(map Param $ ["commit-tree", show tree] ++ ps)
			(L.pack message) repo
	run "update-ref" [Param $ show branch, Param $ show sha] repo
	return sha
	where
		ignorehandle a = snd <$> a
		asString a = L.unpack <$> a
		ps = concatMap (\r -> ["-p", show r]) parentrefs

{- Checks if a string from git config is a true value. -}
configTrue :: String -> Bool
configTrue s = map toLower s == "true"

{- Access to raw config Map -}
configMap :: Repo -> M.Map String String
configMap = config

{- Efficiently looks up a gitattributes value for each file in a list. -}
checkAttr :: String -> [FilePath] -> Repo -> IO [(FilePath, String)]
checkAttr attr files repo = do
	-- git check-attr needs relative filenames input; it will choke
	-- on some absolute filenames. This also means it will output
	-- all relative filenames.
	cwd <- getCurrentDirectory
	let relfiles = map (relPathDirToFile cwd . absPathFrom cwd) files
	(_, fromh, toh) <- hPipeBoth "git" (toCommand params)
        _ <- forkProcess $ do
		hClose fromh
                hPutStr toh $ join "\0" relfiles
                hClose toh
                exitSuccess
        hClose toh
	(map topair . lines) <$> hGetContents fromh
	where
		params = gitCommandLine 
				[ Param "check-attr"
				, Param attr
				, Params "-z --stdin"
				] repo
		topair l = (file, value)
			where 
				file = decodeGitFile $ join sep $ take end bits
				value = bits !! end
				end = length bits - 1
				bits = split sep l
				sep = ": " ++ attr ++ ": "

{- Some git commands output encoded filenames. Decode that (annoyingly
 - complex) encoding. -}
decodeGitFile :: String -> FilePath
decodeGitFile [] = []
decodeGitFile f@(c:s)
	-- encoded strings will be inside double quotes
	| c == '"' = unescape ("", middle)
	| otherwise = f
	where
		e = '\\'
		middle = init s
		unescape (b, []) = b
		-- look for escapes starting with '\'
		unescape (b, v) = b ++ beginning ++ unescape (decode rest)
			where
				pair = span (/= e) v
				beginning = fst pair
				rest = snd pair
		isescape x = x == e
		-- \NNN is an octal encoded character
		decode (x:n1:n2:n3:rest)
			| isescape x && alloctal = (fromoctal, rest)
				where
					alloctal = isOctDigit n1 &&
						isOctDigit n2 &&
						isOctDigit n3
					fromoctal = [chr $ readoctal [n1, n2, n3]]
					readoctal o = read $ "0o" ++ o :: Int
		-- \C is used for a few special characters
		decode (x:nc:rest)
			| isescape x = ([echar nc], rest)
			where
				echar 'a' = '\a'
				echar 'b' = '\b'
				echar 'f' = '\f'
				echar 'n' = '\n'
				echar 'r' = '\r'
				echar 't' = '\t'
				echar 'v' = '\v'
				echar a = a
		decode n = ("", n)

{- Should not need to use this, except for testing decodeGitFile. -}
encodeGitFile :: FilePath -> String
encodeGitFile s = foldl (++) "\"" (map echar s) ++ "\""
	where
		e c = '\\' : [c]
		echar '\a' = e 'a'
		echar '\b' = e 'b'
		echar '\f' = e 'f'
		echar '\n' = e 'n'
		echar '\r' = e 'r'
		echar '\t' = e 't'
		echar '\v' = e 'v'
		echar '\\' = e '\\'
		echar '"'  = e '"'
		echar x
			| ord x < 0x20 = e_num x -- low ascii
			| ord x >= 256 = e_utf x
			| ord x > 0x7E = e_num x -- high ascii
			| otherwise = [x]        -- printable ascii
			where 
				showoctal i = '\\' : printf "%03o" i
				e_num c = showoctal $ ord c
				-- unicode character is decomposed to
				-- Word8s and each is shown in octal
				e_utf c = showoctal =<< (encode [c] :: [Word8])

{- for quickcheck -}
prop_idempotent_deencode :: String -> Bool
prop_idempotent_deencode s = s == decodeGitFile (encodeGitFile s)
