{- Builds distributon info files for each git-annex release in a directory
 - tree, which must itself be part of a git-annex repository. Only files
 - that are present have their info file created.
 -
 - Also gpg signs the files.
 -}

import Common.Annex
import Types.Distribution
import Build.Version
import Utility.UserInfo
import Utility.Path
import qualified Git.Construct
import qualified Annex
import Annex.Content
import Backend
import Git.Command

import Data.Time.Clock

-- git-annex distribution signing key (for Joey Hess)
signingKey :: String
signingKey = "89C809CB"

main = do
	state <- Annex.new =<< Git.Construct.fromPath =<< getRepoDir
	Annex.eval state makeinfos

makeinfos :: Annex ()
makeinfos = do
	version <- liftIO getChangelogVersion
	void $ inRepo $ runBool 
		[ Param "commit"
		, Param "-a"
		, Param "-m"
		, Param $ "publishing git-annex " ++ version
		]
	basedir <- liftIO getRepoDir
	now <- liftIO getCurrentTime
	liftIO $ putStrLn $ "building info files for version " ++ version ++ " in " ++ basedir
	fs <- liftIO $ dirContentsRecursiveSkipping (const False) True (basedir </> "git-annex")
	forM_ fs $ \f -> do
		v <- lookupFile f
		case v of
			Nothing -> noop
			Just k -> whenM (inAnnex k) $ do
				liftIO $ putStrLn f
				let infofile = f ++ ".info"
				liftIO $ writeFile infofile $ show $ GitAnnexDistribution
					{ distributionUrl = mkUrl basedir f
					, distributionKey = k
					, distributionVersion = version
					, distributionReleasedate = now
					, distributionUrgentUpgrade = Nothing
					}
				void $ inRepo $ runBool [Param "add", File infofile]
				signFile infofile
				signFile f
	void $ inRepo $ runBool 
		[ Param "commit"
		, Param "-m"
		, Param $ "updated info files for git-annex " ++ version
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Params "move --to website"
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Params "sync"
		]
	
	{- Check for out of date info files. -}
	infos <- liftIO $ filter (".info" `isSuffixOf`)
		<$> dirContentsRecursive (basedir </> "git-annex")
	ds <- liftIO $ forM infos (readish <$$> readFile)
	let dis = zip infos ds
	let ood = filter (outofdate version) dis
	unless (null ood) $
		error $ "Some info files are out of date: " ++ show (map fst ood)
  where
	outofdate version (_, md) = case md of
		Nothing -> True
		Just d -> distributionVersion d /= version

getRepoDir :: IO FilePath
getRepoDir = do
	home <- liftIO myHomeDir
	return $ home </> "lib" </> "downloads"

mkUrl :: FilePath -> FilePath -> String
mkUrl basedir f = "https://downloads.kitenet.net/" ++ relPathDirToFile basedir f
				
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
