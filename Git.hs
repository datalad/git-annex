{- git repository handling 
 -
 - This is written to be completely independant of git-annex and should be
 - suitable for other uses.
 -
 - Copyright 2010,2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git (
	Repo,
	Ref(..),
	Branch,
	Sha,
	Tag,
	repoFromCwd,
	repoFromAbsPath,
	repoFromUnknown,
	repoFromUrl,
	localToUrl,
	repoIsUrl,
	repoIsSsh,
	repoIsHttp,
	repoIsLocalBare,
	repoDescribe,
	refDescribe,
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
	configGet,
	configMap,
	configRead,
	hConfigRead,
	configStore,
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
	genRemote,
	repoRemoteName,
	repoRemoteNameSet,
	repoRemoteNameFromKey,
	checkAttr,
	decodeGitFile,
	encodeGitFile,
	repoAbsPath,
	reap,
	useIndex,
	getSha,
	shaSize,
	commit,
	assertLocal,

	prop_idempotent_deencode
) where

import System.Posix.Directory
import System.Posix.User
import Control.Exception (bracket_)
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

{- There are two types of repositories; those on local disk and those
 - accessed via an URL. -}
data RepoLocation = Dir FilePath | Url URI | Unknown
	deriving (Show, Eq)

data Repo = Repo {
	location :: RepoLocation,
	config :: M.Map String String,
	remotes :: [Repo],
	-- remoteName holds the name used for this repo in remotes
	remoteName :: Maybe String 
} deriving (Show, Eq)

{- A git ref. Can be a sha1, or a branch or tag name. -}
newtype Ref = Ref String
	deriving (Eq)

instance Show Ref where
	show (Ref v) = v

{- Aliases for Ref. -}
type Branch = Ref
type Sha = Ref
type Tag = Ref

newFrom :: RepoLocation -> Repo
newFrom l = 
	Repo {
		location = l,
		config = M.empty,
		remotes = [],
		remoteName = Nothing
	}

{- Local Repo constructor, requires an absolute path to the repo be
 - specified. -}
repoFromAbsPath :: FilePath -> IO Repo
repoFromAbsPath dir
	| "/" `isPrefixOf` dir = do
 		-- Git always looks for "dir.git" in preference to
		-- to "dir", even if dir ends in a "/".
		let canondir = dropTrailingPathSeparator dir
		let dir' = canondir ++ ".git"
		e <- doesDirectoryExist dir'
		if e
			then ret dir'
			else if "/.git" `isSuffixOf` canondir
				then do
					-- When dir == "foo/.git", git looks
					-- for "foo/.git/.git", and failing
					-- that, uses "foo" as the repository.
					e' <- doesDirectoryExist $ dir </> ".git"
					if e'
						then ret dir
						else ret $ takeDirectory canondir
				else ret dir
	| otherwise = error $ "internal error, " ++ dir ++ " is not absolute"
	where
		ret = return . newFrom . Dir

{- Remote Repo constructor. Throws exception on invalid url. -}
repoFromUrl :: String -> IO Repo
repoFromUrl url
	| startswith "file://" url = repoFromAbsPath $ uriPath u
	| otherwise = return $ newFrom $ Url u
		where
			u = fromMaybe bad $ parseURI url
			bad = error $ "bad url " ++ url

{- Creates a repo that has an unknown location. -}
repoFromUnknown :: Repo
repoFromUnknown = newFrom Unknown

{- Converts a Local Repo into a remote repo, using the reference repo
 - which is assumed to be on the same host. -}
localToUrl :: Repo -> Repo -> Repo
localToUrl reference r
	| not $ repoIsUrl reference = error "internal error; reference repo not url"
	| repoIsUrl r = r
	| otherwise = r { location = Url $ fromJust $ parseURI absurl }
	where
		absurl =
			urlScheme reference ++ "//" ++
			urlAuthority reference ++
			workTree r

{- User-visible description of a git repo. -}
repoDescribe :: Repo -> String
repoDescribe Repo { remoteName = Just name } = name
repoDescribe Repo { location = Url url } = show url
repoDescribe Repo { location = Dir dir } = dir
repoDescribe Repo { location = Unknown } = "UNKNOWN"

{- Converts a fully qualified git ref into a user-visible version. -}
refDescribe :: Ref -> String
refDescribe = remove "refs/heads/" . remove "refs/remotes/" . show
	where
		remove prefix s
			| prefix `isPrefixOf` s = drop (length prefix) s
			| otherwise = s

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
		(':':p) -> Just (read p)
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
 - with the specified parent refs. -}
commit :: String -> Ref -> [Ref] -> Repo -> IO ()
commit message newref parentrefs repo = do
	tree <- getSha "write-tree" $ asString $
		pipeRead [Param "write-tree"] repo
	sha <- getSha "commit-tree" $ asString $
		ignorehandle $ pipeWriteRead
			(map Param $ ["commit-tree", show tree] ++ ps)
			(L.pack message) repo
	run "update-ref" [Param $ show newref, Param $ show sha] repo
	where
		ignorehandle a = snd <$> a
		asString a = L.unpack <$> a
		ps = concatMap (\r -> ["-p", show r]) parentrefs

{- Runs git config and populates a repo with its config. -}
configRead :: Repo -> IO Repo
configRead repo@(Repo { location = Dir d }) = do
	{- Cannot use pipeRead because it relies on the config having
	   been already read. Instead, chdir to the repo. -}
	cwd <- getCurrentDirectory
	bracket_ (changeWorkingDirectory d) (changeWorkingDirectory cwd) $
		pOpen ReadFromPipe "git" ["config", "--list"] $ hConfigRead repo
configRead r = assertLocal r $ error "internal"

{- Reads git config from a handle and populates a repo with it. -}
hConfigRead :: Repo -> Handle -> IO Repo
hConfigRead repo h = do
	val <- hGetContentsStrict h
	configStore val repo

{- Stores a git config into a repo, returning the new version of the repo.
 - The git config may be multiple lines, or a single line. Config settings
 - can be updated inrementally. -}
configStore :: String -> Repo -> IO Repo
configStore s repo = do
	let repo' = repo { config = configParse s `M.union` config repo }
	rs <- configRemotes repo'
	return $ repo' { remotes = rs }

{- Parses git config --list output into a config map. -}
configParse :: String -> M.Map String String
configParse s = M.fromList $ map pair $ lines s
	where
		pair = separate (== '=')

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
configRemotes :: Repo -> IO [Repo]
configRemotes repo = mapM construct remotepairs
	where
		filterconfig f = filter f $ M.toList $ config repo
		filterkeys f = filterconfig (\(k,_) -> f k)
		remotepairs = filterkeys isremote
		isremote k = startswith "remote." k && endswith ".url" k
		construct (k,v) = repoRemoteNameFromKey k <$> genRemote v repo

{- Generates one of a repo's remotes using a given location (ie, an url). -}
genRemote :: String -> Repo -> IO Repo
genRemote s repo = gen $ calcloc s
	where
		filterconfig f = filter f $ M.toList $ config repo
		gen v	
			| scpstyle v = repoFromUrl $ scptourl v
			| isURI v = repoFromUrl v
			| otherwise = repoFromRemotePath v repo
		-- insteadof config can rewrite remote location
		calcloc l
			| null insteadofs = l
			| otherwise = replacement ++ drop (length bestvalue) l
			where
				replacement = drop (length prefix) $
					take (length bestkey - length suffix) bestkey
				(bestkey, bestvalue) = maximumBy longestvalue insteadofs
				longestvalue (_, a) (_, b) = compare b a
				insteadofs = filterconfig $ \(k, v) -> 
					startswith prefix k &&
					endswith suffix k &&
					startswith v l
				(prefix, suffix) = ("url." , ".insteadof")
		-- git remotes can be written scp style -- [user@]host:dir
		scpstyle v = ":" `isInfixOf` v && not ("//" `isInfixOf` v)
		scptourl v = "ssh://" ++ host ++ slash dir
			where
				(host, dir) = separate (== ':') v
				slash d	| d == "" = "/~/" ++ d
					| "/" `isPrefixOf` d = d
					| "~" `isPrefixOf` d = '/':d
					| otherwise = "/~/" ++ d

{- Checks if a string from git config is a true value. -}
configTrue :: String -> Bool
configTrue s = map toLower s == "true"

{- Returns a single git config setting, or a default value if not set. -}
configGet :: String -> String -> Repo -> String
configGet key defaultValue repo = 
	M.findWithDefault defaultValue key (config repo)

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

{- Constructs a Repo from the path specified in the git remotes of
 - another Repo. -}
repoFromRemotePath :: FilePath -> Repo -> IO Repo
repoFromRemotePath dir repo = do
	dir' <- expandTilde dir
	repoFromAbsPath $ workTree repo </> dir'

{- Git remotes can have a directory that is specified relative
 - to the user's home directory, or that contains tilde expansions.
 - This converts such a directory to an absolute path.
 - Note that it has to run on the system where the remote is.
 -}
repoAbsPath :: FilePath -> IO FilePath
repoAbsPath d = do
	d' <- expandTilde d
	h <- myHomeDir
	return $ h </> d'

expandTilde :: FilePath -> IO FilePath
expandTilde = expandt True
	where
		expandt _ [] = return ""
		expandt _ ('/':cs) = do
			v <- expandt True cs
			return ('/':v)
		expandt True ('~':'/':cs) = do
			h <- myHomeDir
			return $ h </> cs
		expandt True ('~':cs) = do
			let (name, rest) = findname "" cs
			u <- getUserEntryForName name
			return $ homeDirectory u </> rest
		expandt _ (c:cs) = do
			v <- expandt False cs
			return (c:v)
		findname n [] = (n, "")
		findname n (c:cs)
			| c == '/' = (n, cs)
			| otherwise = findname (n++[c]) cs

{- Finds the current git repository, which may be in a parent directory. -}
repoFromCwd :: IO Repo
repoFromCwd = getCurrentDirectory >>= seekUp isRepoTop >>= maybe norepo makerepo
	where
		makerepo = return . newFrom . Dir
		norepo = error "Not in a git repository."

seekUp :: (FilePath -> IO Bool) -> FilePath -> IO (Maybe FilePath)
seekUp want dir = do
	ok <- want dir
	if ok
		then return $ Just dir
		else case parentDir dir of
			"" -> return Nothing
			d -> seekUp want d

isRepoTop :: FilePath -> IO Bool
isRepoTop dir = do
	r <- isRepo
	b <- isBareRepo
	return (r || b)
	where
		isRepo = gitSignature ".git" ".git/config"
		isBareRepo = gitSignature "objects" "config"
		gitSignature subdir file = liftM2 (&&)
			(doesDirectoryExist (dir ++ "/" ++ subdir))
			(doesFileExist (dir ++ "/" ++ file))
