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
	repoFromCwd,
	repoFromAbsPath,
	repoFromUnknown,
	repoFromUrl,
	localToUrl,
	repoIsUrl,
	repoIsSsh,
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
	attributes,
	remotes,
	remotesAdd,
	repoRemoteName,
	repoRemoteNameSet,
	checkAttr,
	decodeGitFile,
	encodeGitFile,
	repoAbsPath,
	reap,
	useIndex,
	hashObject,
	getSha,
	shaSize,
	commit,

	prop_idempotent_deencode
) where

import Control.Monad (unless, when)
import System.Directory
import System.FilePath
import System.Posix.Directory
import System.Posix.User
import System.Posix.Process
import System.Path
import System.Cmd.Utils
import IO (bracket_, try)
import Data.String.Utils
import System.IO
import qualified Data.Map as M hiding (map, split)
import Network.URI
import Data.Maybe
import Data.Char
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encode)
import Text.Printf
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import System.Exit
import System.Posix.Env (setEnv, unsetEnv, getEnv)

import Utility

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

{- Sets the name of a remote based on the git config key, such as
   "remote.foo.url". -}
repoRemoteNameSet :: Repo -> String -> Repo
repoRemoteNameSet r k = r { remoteName = Just basename }
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

{- Path to a repository's .git directory, relative to its workTree. -}
gitDir :: Repo -> String
gitDir repo
	| configBare repo = ""
	| otherwise = ".git"

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
workTreeFile :: Repo -> FilePath -> IO FilePath
workTreeFile repo@(Repo { location = Dir d }) file = do
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
workTreeFile repo _ = assertLocal repo $ error "internal"

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
urlAuthority Repo { location = Url u } = uriUserInfo a ++ uriRegName' a ++ uriPort a
	where
		a = fromMaybe (error $ "bad url " ++ show u) (uriAuthority u)
urlAuthority repo = assertUrl repo $ error "internal"

{- Applies a function to extract part of the uriAuthority of an URL repo. -}
urlAuthPart :: (URIAuth -> a) -> Repo -> a
urlAuthPart a Repo { location = Url u } = a auth
	where
		auth = fromMaybe (error $ "bad url " ++ show u) (uriAuthority u)
urlAuthPart _ repo = assertUrl repo $ error "internal"

{- Constructs a git command line operating on the specified repo. -}
gitCommandLine :: Repo -> [CommandParam] -> [CommandParam]
gitCommandLine repo@(Repo { location = Dir d} ) params =
	-- force use of specified repo via --git-dir and --work-tree
	[ Param ("--git-dir=" ++ d ++ "/" ++ gitDir repo)
	, Param ("--work-tree=" ++ d)
	] ++ params
gitCommandLine repo _ = assertLocal repo $ error "internal"

{- Runs git in the specified repo. -}
runBool :: Repo -> String -> [CommandParam] -> IO Bool
runBool repo subcommand params = assertLocal repo $
	boolSystem "git" $ gitCommandLine repo $ Param subcommand : params

{- Runs git in the specified repo, throwing an error if it fails. -}
run :: Repo -> String -> [CommandParam] -> IO ()
run repo subcommand params = assertLocal repo $
	runBool repo subcommand params
		>>! error $ "git " ++ show params ++ " failed"

{- Runs a git subcommand and returns its output, lazily. 
 -
 - Note that this leaves the git process running, and so zombies will
 - result unless reap is called.
 -}
pipeRead :: Repo -> [CommandParam] -> IO String
pipeRead repo params = assertLocal repo $ do
	(_, s) <- pipeFrom "git" $ toCommand $ gitCommandLine repo params
	return s

{- Runs a git subcommand, feeding it input.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWrite :: Repo -> [CommandParam] -> String -> IO PipeHandle
pipeWrite repo params s = assertLocal repo $
	pipeTo "git" (toCommand $ gitCommandLine repo params) s

{- Runs a git subcommand, feeding it input, and returning its output.
 - You should call either getProcessStatus or forceSuccess on the PipeHandle. -}
pipeWriteRead :: Repo -> [CommandParam] -> String -> IO (PipeHandle, String)
pipeWriteRead repo params s = assertLocal repo $
	pipeBoth "git" (toCommand $ gitCommandLine repo params) s

{- Reaps any zombie git processes. -}
reap :: IO ()
reap = do
	-- throws an exception when there are no child processes
	r <- catch (getAnyProcessStatus False True) (\_ -> return Nothing)
	maybe (return ()) (const reap) r

{- Forces git to use the specified index file.
 - Returns an action that will reset back to the default
 - index file. -}
useIndex :: FilePath -> IO (IO ())
useIndex index = do
	res <- try $ getEnv var
	setEnv var index True
	return $ reset res
	where
		var = "GIT_INDEX_FILE"
		reset (Right (Just v)) = setEnv var v True
		reset _ = unsetEnv var

{- Injects some content into git, returning its hash. -}
hashObject :: Repo -> String -> IO String
hashObject repo content = getSha subcmd $ do
	(h, s) <- pipeWriteRead repo (map Param params) content
	length s `seq` do
		forceSuccess h
		reap -- XXX unsure why this is needed
		return s
	where
		subcmd = "hash-object"
		params = [subcmd, "-w", "--stdin"]

{- Runs an action that causes a git subcommand to emit a sha, and strips
   any trailing newline, returning the sha. -}
getSha :: String -> IO String -> IO String
getSha subcommand a = do
	t <- a
	let t' = if last t == '\n'
		then take (length t - 1) t
		else t
	when (length t' /= shaSize) $
		error $ "failed to read sha from git " ++ subcommand ++ " (" ++ t' ++ ")"
	return t'

{- Size of a git sha. -}
shaSize :: Int
shaSize = 40

{- Commits the index into the specified branch, 
 - with the specified parent refs. -}
commit :: Repo -> String -> String -> [String] -> IO ()
commit g message newref parentrefs = do
	tree <- getSha "write-tree" $
		pipeRead g [Param "write-tree"]
	sha <- getSha "commit-tree" $ ignorehandle $
		pipeWriteRead g (map Param $ ["commit-tree", tree] ++ ps) message
	run g "update-ref" [Param newref, Param sha]
	where
		ignorehandle a = return . snd =<< a
		ps = concatMap (\r -> ["-p", r]) parentrefs

{- Reads null terminated output of a git command (as enabled by the -z 
 - parameter), and splits it into a list of files/lines/whatever. -}
pipeNullSplit :: Repo -> [CommandParam] -> IO [FilePath]
pipeNullSplit repo params = do
	fs0 <- pipeRead repo params
	return $ split0 fs0
	where
		split0 s = filter (not . null) $ split "\0" s

{- Runs git config and populates a repo with its config. -}
configRead :: Repo -> IO Repo
configRead repo@(Repo { location = Dir d }) = do
	{- Cannot use pipeRead because it relies on the config having
	   been already read. Instead, chdir to the repo. -}
	cwd <- getCurrentDirectory
	bracket_ (changeWorkingDirectory d)
		(\_ -> changeWorkingDirectory cwd) $
			pOpen ReadFromPipe "git" ["config", "--list"] $
				hConfigRead repo
configRead r = assertLocal r $ error "internal"

{- Reads git config from a handle and populates a repo with it. -}
hConfigRead :: Repo -> Handle -> IO Repo
hConfigRead repo h = do
	val <- hGetContentsStrict h
	configStore repo val

{- Stores a git config into a repo, returning the new version of the repo.
 - The git config may be multiple lines, or a single line. Config settings
 - can be updated inrementally. -}
configStore :: Repo -> String -> IO Repo
configStore repo s = do
	let repo' = repo { config = configParse s `M.union` config repo }
	rs <- configRemotes repo'
	return $ repo' { remotes = rs }

{- Parses git config --list output into a config map. -}
configParse :: String -> M.Map String String
configParse s = M.fromList $ map pair $ lines s
	where
		pair l = (key l, val l)
		key l = head $ keyval l
		val l = join sep $ drop 1 $ keyval l
		keyval l = split sep l :: [String]
		sep = "="

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
configRemotes :: Repo -> IO [Repo]
configRemotes repo = mapM construct remotepairs
	where
		remotepairs = M.toList $ filterremotes $ config repo
		filterremotes = M.filterWithKey (\k _ -> isremote k)
		isremote k = startswith "remote." k && endswith ".url" k
		construct (k,v) = do
			r <- gen v
			return $ repoRemoteNameSet r k
		gen v	| scpstyle v = repoFromUrl $ scptourl v
			| isURI v = repoFromUrl v
			| otherwise = repoFromRemotePath v repo
		-- git remotes can be written scp style -- [user@]host:dir
		scpstyle v = ":" `isInfixOf` v && not ("//" `isInfixOf` v)
		scptourl v = "ssh://" ++ host ++ slash dir
			where
				bits = split ":" v
				host = head bits
				dir = join ":" $ drop 1 bits
				slash d	| d == "" = "/~/" ++ dir
					| head d == '/' = dir
					| head d == '~' = '/':dir
					| otherwise = "/~/" ++ dir

{- Checks if a string from git config is a true value. -}
configTrue :: String -> Bool
configTrue s = map toLower s == "true"

{- Returns a single git config setting, or a default value if not set. -}
configGet :: Repo -> String -> String -> String
configGet repo key defaultValue = 
	M.findWithDefault defaultValue key (config repo)

{- Access to raw config Map -}
configMap :: Repo -> M.Map String String
configMap = config

{- Efficiently looks up a gitattributes value for each file in a list. -}
checkAttr :: Repo -> String -> [FilePath] -> IO [(FilePath, String)]
checkAttr repo attr files = do
	-- git check-attr wants files that are absolute (or relative to the
	-- top of the repo). But we're passed files relative to the current
	-- directory. Convert to absolute, and then convert the filenames
	-- in its output back to relative.
	cwd <- getCurrentDirectory
	let top = workTree repo
	let absfiles = map (absPathFrom cwd) files
	(_, fromh, toh) <- hPipeBoth "git" (toCommand params)
        _ <- forkProcess $ do
		hClose fromh
                hPutStr toh $ join "\0" absfiles
                hClose toh
                exitSuccess
        hClose toh
	s <- hGetContents fromh
	return $ map (topair cwd top) $ lines s
	where
		params = gitCommandLine repo [Param "check-attr", Param attr, Params "-z --stdin"]
		topair cwd top l = (relfile, value)
			where 
				relfile
					| startswith cwd' file = drop (length cwd') file
					| otherwise = relPathDirToFile top' file
				file = decodeGitFile $ join sep $ take end bits
				value = bits !! end
				end = length bits - 1
				bits = split sep l
				sep = ": " ++ attr ++ ": "
				cwd' = cwd ++ "/"
				top' = top ++ "/"

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
		middle = take (length s - 1) s
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
		gitSignature subdir file = do
			s <- (doesDirectoryExist (dir ++ "/" ++ subdir))
			f <- (doesFileExist (dir ++ "/" ++ file))
			return (s && f)
