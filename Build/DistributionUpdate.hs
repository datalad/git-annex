{- Downloads git-annex autobuilds and installs them into the git-annex
 - repository in ~/lib/downloads that is used to distribute git-annex
 - releases.
 -
 - Generates info files, containing the version (of the corresponding file
 - from the autobuild).
 -
 - Builds standalone rpms from the standalone tarballs, and populates
 - a rpm package repository with them using the createrepo_c program.
 -
 - Also gpg signs the files.
 -}

import Annex.Common
import Types.Distribution
import Build.Version (getChangelogVersion, Version)
import Utility.UserInfo
import Utility.Url
import Utility.Tmp.Dir
import Utility.Metered
import qualified Git.Construct
import qualified Annex
import Annex.Content
import Annex.WorkTree
import Git.Command
import qualified Utility.RawFilePath as R
import Annex.Action

import Data.Time.Clock
import Data.Char
import Data.Either
import System.Posix.Directory

-- git-annex distribution signing key (for Joey Hess)
signingKey :: String
signingKey = "89C809CB"

-- URL to an autobuilt git-annex file, and the place to install
-- it in the repository.
autobuilds :: [(URLString, FilePath)]
autobuilds = 
	(map linuxarch ["i386", "amd64", "armel", "arm64", "i386-ancient", "arm64-ancient"]) ++
	[ (autobuild "x86_64-apple-catalina/git-annex.dmg", "git-annex/OSX/current/10.15_Catalina/git-annex.dmg")
	, (autobuild "windows/git-annex-installer.exe", "git-annex/windows/current/git-annex-installer.exe")
	]
  where
	linuxarch a =
		( autobuild (a ++ "/git-annex-standalone-" ++ a ++ ".tar.gz")
		, "git-annex/linux/current/git-annex-standalone-" ++ a ++ ".tar.gz"
		)
	autobuild f = "https://downloads.kitenet.net/git-annex/autobuild/" ++ f

-- Names of architectures in standalone tarballs and the corresponding
-- rpm architecture.
tarrpmarches :: [(String, String)]
tarrpmarches =
	[ ("i386", "i386")
	, ("amd64", "x86_64")
	, ("arm64", "aarch64")
	]

main :: IO ()
main = do
	useFileSystemEncoding
	version <- getChangelogVersion
	repodir <- getRepoDir
	topdir <- getCurrentDirectory
	changeWorkingDirectory repodir
	updated <- catMaybes <$> mapM (getbuild repodir) autobuilds
	state <- Annex.new =<< Git.Construct.fromPath (toRawFilePath ".")
	ood <- Annex.eval state $ do
		buildrpms topdir updated
		is <- makeinfos updated version
		quiesce False
		return is
	syncToArchiveOrg
	unless (null ood) $
		error $ "Some info files are out of date: " ++ show (map fst ood)

-- Download a build from the autobuilder, virus check it, and return its
-- version.
-- It's very important that the version matches the build, otherwise
-- auto-upgrades can loop reatedly. So, check build-version before
-- and after downloading the file.
getbuild :: FilePath -> (URLString, FilePath) -> IO (Maybe (FilePath, Version))
getbuild repodir (url, f) = do
	bv1 <- getbv
	let dest = repodir </> f
	let tmp = dest ++ ".tmp"
	removeWhenExistsWith removeFile tmp
	createDirectoryIfMissing True (fromRawFilePath (parentDir (toRawFilePath dest)))
	let oops s = do
		removeWhenExistsWith removeFile tmp
		putStrLn $ "*** " ++ s
		return Nothing
	uo <- defUrlOptions
	ifM (isRight <$> download nullMeterUpdate Nothing url tmp uo)
		( ifM (liftIO $ virusFree tmp)
			( do
				bv2 <- getbv
				case bv2 of
					Nothing -> oops $ "no build-version file for " ++ url
					(Just v)
						| bv2 == bv1 -> do
							removeWhenExistsWith removeFile dest
							renameFile tmp dest
							-- remove git rev part of version
							let v' = takeWhile (/= '-') v
							return $ Just (f, v')
						| otherwise -> oops $ "build version changed while downloading " ++ url ++ " " ++ show (bv1, bv2)
			, oops $ "VIRUS detected in " ++ url
			)
		, oops $ "failed to download " ++ url
		)
  where
	bvurl = takeDirectory url ++ "/build-version"
	getbv = do
		bv <- catchDefaultIO "" $ readProcess "curl" ["--silent", bvurl]
		return $ if null bv || any (not . versionchar) bv then Nothing else Just bv
	versionchar c = isAlphaNum c || c == '.' || c == '-'

makeinfos :: [(FilePath, Version)] -> Version -> Annex [([Char], Maybe GitAnnexDistribution)]
makeinfos updated changelogversion = do
	mapM_ (\f -> inRepo $ runBool [Param "annex", Param "add", File f]) (map fst updated)
	void $ inRepo $ runBool 
		[ Param "commit"
		, Param "-a"
		, Param ("-S" ++ signingKey)
		, Param "-m"
		, Param $ "publishing git-annex " ++ descversion
		]
	now <- liftIO getCurrentTime
	liftIO $ putStrLn $ "building info files"
	forM_ updated $ \(f, bv) -> do
		v <- lookupKey (toRawFilePath f)
		case v of
			Nothing -> noop
			Just k -> whenM (inAnnex k) $ do
				liftIO $ putStrLn f
				let infofile = f ++ ".info"
				let d = GitAnnexDistribution
					{ distributionUrl = mkUrl f
					, distributionKey = fromKey id k
					, distributionVersion = bv
					, distributionReleasedate = now
					, distributionUrgentUpgrade = Just "6.20180626"
					}
				liftIO $ writeFile infofile $ formatInfoFile d
				void $ inRepo $ runBool [Param "add", File infofile]
				signFile infofile
				signFile f
	void $ inRepo $ runBool 
		[ Param "commit"
		, Param ("-S" ++ signingKey)
		, Param "-m"
		, Param $ "updated info files for git-annex " ++ descversion
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Param "move"
		, Param "--to"
		, Param "website"
		, Param "--force"
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Param "sync"
		]
	
	-- Check for out of date info files.
	infos <- liftIO $ filter (".info" `isSuffixOf`)
		<$> emptyWhenDoesNotExist (dirContentsRecursive "git-annex")
	ds <- liftIO $ forM infos (readish <$$> readFile)
	let dis = zip infos ds
	let ood = filter outofdate dis
	return ood
  where
	outofdate (_, md) = case md of
		Nothing -> True
		Just d -> distributionVersion d /= changelogversion
	descversion = unwords (nub (map snd updated))

getRepoDir :: IO FilePath
getRepoDir = do
	home <- liftIO myHomeDir
	return $ home </> "lib" </> "downloads"

mkUrl :: FilePath -> String
mkUrl f = "https://downloads.kitenet.net/" ++ f
				
signFile :: FilePath -> Annex ()
signFile f = do
	void $ liftIO $ boolSystem "gpg"
		[ Param "-a"
		, Param $ "--default-key=" ++ signingKey
		, Param "--detach-sign"
		, File f
		]
	liftIO $ R.rename (toRawFilePath (f ++ ".asc")) (toRawFilePath (f ++ ".sig"))
	void $ inRepo $ runBool [Param "add", File (f ++ ".sig")]

-- clamscan should handle unpacking archives, but did not in my
-- testing, so do it manually.
virusFree :: FilePath -> IO Bool
virusFree f 
	| ".tar.gz" `isSuffixOf` f = unpack $ \tmpdir ->
		boolSystem "tar" [ Param "xf", File f, Param "-C", File tmpdir ]
	| ".dmg" `isSuffixOf` f = unpack $ \tmpdir -> do
		-- 7z can extract partitions from a dmg, and then
		-- run on partitions can extract their files
		unhfs tmpdir f
		parts <- filter (".hfs" `isSuffixOf`) <$> getDirectoryContents tmpdir
		forM_ parts $ unhfs tmpdir
		return True
	| otherwise = clamscan f
  where
	clamscan f' = boolSystem "clamscan"
		[ Param "--no-summary"
		, Param "-r"
		, Param f'
		]
	unpack unpacker = withTmpDir "clamscan" $ \tmpdir -> do
		unlessM (unpacker tmpdir) $
			error $ "Failed to unpack " ++ f ++ " for virus scan"
		clamscan tmpdir
	unhfs dest f' = unlessM (boolSystem "7z" [ Param "x", Param ("-o" ++ dest), File f' ]) $
		error $ "Failed extracting hfs " ++ f'

buildrpms :: FilePath -> [(FilePath, Version)] -> Annex ()
buildrpms topdir l = do
	liftIO $ createDirectoryIfMissing True rpmrepo
	oldrpms <- map (rpmrepo </>) . filter (".rpm" `isSuffixOf`)
		<$> liftIO (getDirectoryContents rpmrepo)
	forM_ tarrpmarches $ \(tararch, rpmarch) ->
		forM_ (filter (isstandalonetarball tararch . fst) l) $ \(tarball, v) -> do
			liftIO $ mapM_ (removeWhenExistsWith (R.removeLink . toRawFilePath))
				(filter ((rpmarch ++ ".rpm") `isSuffixOf`) oldrpms)
			void $ liftIO $ boolSystem script 
				[ Param rpmarch
				, File tarball
				, Param v
				, File rpmrepo
				]
	void $ inRepo $ runBool [Param "annex", Param "get", File rpmrepo]
	void $ liftIO $ boolSystem "createrepo_c" [File rpmrepo]
	void $ inRepo $ runBool [Param "annex", Param "add", File rpmrepo]
  where
	isstandalonetarball tararch f =
		("git-annex-standalone-" ++ tararch ++ ".tar.gz") `isSuffixOf` f
	script = topdir </> "standalone" </> "rpm" </> "rpmbuild-from-standalone-tarball"
	rpmrepo = "git-annex/linux/current/rpms"

-- My .mrconfig is configured to copy new files to archive.org,
-- and moves old versions of content to archive.org to free up space on my
-- server.
syncToArchiveOrg :: IO ()
syncToArchiveOrg = void $ boolSystem "mr" 
	[ Param "-d"
	, File "/srv/web/downloads.kitenet.net"
	, Param "update"
	]
