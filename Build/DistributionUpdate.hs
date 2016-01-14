{- Downloads git-annex autobuilds and installs them into the git-annex
 - repository in ~/lib/downloads that is used to distribute git-annex
 - releases.
 -
 - Generates info files, containing the version (of the corresponding file
 - from the autobuild).
 -
 - Also gpg signs the files.
 -}

import Common.Annex
import Types.Distribution
import Build.Version (getChangelogVersion, Version)
import Utility.UserInfo
import Utility.Url
import qualified Git.Construct
import qualified Annex
import Annex.Content
import Annex.WorkTree
import Git.Command

import Data.Time.Clock
import Data.Char
import System.Posix.Directory

-- git-annex distribution signing key (for Joey Hess)
signingKey :: String
signingKey = "89C809CB"

-- URL to an autobuilt git-annex file, and the place to install
-- it in the repository.
autobuilds :: [(URLString, FilePath)]
autobuilds = 
	(map linuxarch ["i386", "amd64", "armel", "i386-ancient"]) ++
	(map androidversion ["4.0", "4.3", "5.0"]) ++
	[ (autobuild "x86_64-apple-yosemite/git-annex.dmg", "git-annex/OSX/current/10.10_Yosemite/git-annex.dmg")
	, (autobuild "windows/git-annex-installer.exe", "git-annex/windows/current/git-annex-installer.exe")
	]
  where
	linuxarch a =
		( autobuild (a ++ "/git-annex-standalone-" ++ a ++ ".tar.gz")
		, "git-annex/linux/current/git-annex-standalone-" ++ a ++ ".tar.gz"
		)
	androidversion v =
		( autobuild ("android/" ++ v ++ "/git-annex.apk")
		, "git-annex/android/current/" ++ v ++ "/git-annex.apk"
		)
	autobuild f = "https://downloads.kitenet.net/git-annex/autobuild/" ++ f

main :: IO ()
main = do
	version <- liftIO getChangelogVersion
	repodir <- getRepoDir
	changeWorkingDirectory repodir
	updated <- catMaybes <$> mapM (getbuild repodir) autobuilds
	state <- Annex.new =<< Git.Construct.fromPath "."
	Annex.eval state (makeinfos updated version)

-- Download a build from the autobuilder, and return its version.
-- It's very important that the version matches the build, otherwise
-- auto-upgrades can loop reatedly. So, check build-version before
-- and after downloading the file.
getbuild :: FilePath -> (URLString, FilePath) -> IO (Maybe (FilePath, Version))
getbuild repodir (url, f) = do
	bv1 <- getbv
	let dest = repodir </> f
	let tmp = dest ++ ".tmp"
	nukeFile tmp
	createDirectoryIfMissing True (parentDir dest)
	let oops s = do
		nukeFile tmp
		putStrLn $ "*** " ++ s
		return Nothing
	ifM (download url tmp def)
		( do
			bv2 <- getbv
			case bv2 of
				Nothing -> oops $ "no build-version file for " ++ url
				(Just v)
					| bv2 == bv1 -> do
						nukeFile dest
						renameFile tmp dest
						-- remove git rev part of version
						let v' = takeWhile (/= '-') v
						return $ Just (f, v')
					| otherwise -> oops $ "build version changed while downloading " ++ url ++ " " ++ show (bv1, bv2)
		, oops $ "failed to download " ++ url
		)
  where
	bvurl = takeDirectory url ++ "/build-version"
	getbv = do
		bv <- catchDefaultIO "" $ readProcess "curl" ["--silent", bvurl]
		return $ if null bv || any (not . versionchar) bv then Nothing else Just bv
	versionchar c = isAlphaNum c || c == '.' || c == '-'

makeinfos :: [(FilePath, Version)] -> Version -> Annex ()
makeinfos updated version = do
	mapM_ (\f -> inRepo $ runBool [Param "annex", Param "add", File f]) (map fst updated)
	void $ inRepo $ runBool 
		[ Param "commit"
		, Param "-a"
		, Param ("-S" ++ signingKey)
		, Param "-m"
		, Param $ "publishing git-annex " ++ version
		]
	now <- liftIO getCurrentTime
	liftIO $ putStrLn $ "building info files"
	forM_ updated $ \(f, bv) -> do
		v <- lookupFile f
		case v of
			Nothing -> noop
			Just k -> whenM (inAnnex k) $ do
				liftIO $ putStrLn f
				let infofile = f ++ ".info"
				liftIO $ writeFile infofile $ show $ GitAnnexDistribution
					{ distributionUrl = mkUrl f
					, distributionKey = k
					, distributionVersion = bv
					, distributionReleasedate = now
					, distributionUrgentUpgrade = Nothing
					}
				void $ inRepo $ runBool [Param "add", File infofile]
				signFile infofile
				signFile f
	void $ inRepo $ runBool 
		[ Param "commit"
		, Param ("-S" ++ signingKey)
		, Param "-m"
		, Param $ "updated info files for git-annex " ++ version
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Param "move"
		, Param "--to"
		, Param "website"
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Param "sync"
		]
	
	-- Check for out of date info files.
	infos <- liftIO $ filter (".info" `isSuffixOf`)
		<$> dirContentsRecursive "git-annex"
	ds <- liftIO $ forM infos (readish <$$> readFile)
	let dis = zip infos ds
	let ood = filter outofdate dis
	unless (null ood) $
		error $ "Some info files are out of date: " ++ show (map fst ood)
  where
	outofdate (_, md) = case md of
		Nothing -> True
		Just d -> distributionVersion d /= version

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
	liftIO $ rename (f ++ ".asc") (f ++ ".sig")
	void $ inRepo $ runBool [Param "add", File (f ++ ".sig")]
