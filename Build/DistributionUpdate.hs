{- Builds distributon info files for each git-annex release in a directory
 - tree, which must itself be part of a git-annex repository. Only files
 - that are present have their info file created. -}

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

main = do
	state <- Annex.new =<< Git.Construct.fromPath =<< getRepoDir
	Annex.eval state makeinfos

makeinfos :: Annex ()
makeinfos = do
	basedir <- liftIO getRepoDir
	version <- liftIO getChangelogVersion
	now <- liftIO getCurrentTime
	liftIO $ putStrLn $ "building info files for version " ++ version ++ " in " ++ basedir
	fs <- liftIO $ dirContentsRecursiveSkipping (== "info") True (basedir </> "git-annex")
	forM_ fs $ \f -> do
		v <- lookupFile f
		case v of
			Nothing -> noop
			Just (k, _b) -> whenM (inAnnex k) $ do
				liftIO $ putStrLn f
				let infofile = f ++ ".info"
				liftIO $ writeFile infofile $ show $ GitAnnexDistribution
					{ distributionUrl = mkUrl basedir f
					, distributionKey = k
					, distributionVersion = version
					, distributionReleasedate = now
					, distributionUrgentUpgrade = Nothing
					}
				void $ inRepo $ runBool [Param "add", Param infofile]
	void $ inRepo $ runBool 
		[ Param "commit"
		, Param "-m"
		, Param $ "publishing git-annex " ++ version
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Params "move --to website"
		]
	void $ inRepo $ runBool
		[ Param "annex"
		, Params "sync"
		]

getRepoDir :: IO FilePath
getRepoDir = do
	home <- liftIO myHomeDir
	return $ home </> "lib" </> "downloads"

mkUrl :: FilePath -> FilePath -> String
mkUrl basedir f = "https://downloads.kitenet.net/" ++ relPathDirToFile basedir f
