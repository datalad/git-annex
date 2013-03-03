{- git-annex assistant webapp configurators for making local repositories
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

#if defined VERSION_yesod_form
#if ! MIN_VERSION_yesod_form(1,2,0)
#define WITH_OLD_YESOD
#endif
#endif

module Assistant.WebApp.Configurators.Local where

import Assistant.WebApp.Common
import Assistant.WebApp.Utility
import Assistant.WebApp.OtherRepos
import Assistant.MakeRemote
import Init
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Command
import qualified Annex
import Locations.UserConfig
import Utility.FreeDesktop
import Utility.Mounts
import Utility.DiskFree
import Utility.DataUnits
import Utility.Network
import Remote (prettyListUUIDs)
import Annex.UUID
import Types.StandardGroups
import Logs.PreferredContent
import Utility.UserInfo
import Config

import qualified Data.Text as T
import Data.Char
import System.Posix.Directory
import qualified Control.Exception as E

data RepositoryPath = RepositoryPath Text
	deriving Show

{- Custom field display for a RepositoryPath, with an icon etc.
 -
 - Validates that the path entered is not empty, and is a safe value
 - to use as a repository. -}
repositoryPathField :: forall sub. Bool -> Field sub WebApp Text
repositoryPathField autofocus = Field
#ifdef WITH_OLD_YESOD
	{ fieldParse = parse
#else
	{ fieldParse = \l _ -> parse l
#endif
	, fieldView = view
#ifndef WITH_OLD_YESOD
	, fieldEnctype = UrlEncoded
#endif
	}
  where
	view idAttr nameAttr attrs val isReq =
		[whamlet|<input type="text" *{attrs} id="#{idAttr}" name="#{nameAttr}" :isReq:required :autofocus:autofocus value="#{either id id val}">|]

	parse [path]
		| T.null path = nopath
		| otherwise = liftIO $ checkRepositoryPath path
	parse [] = return $ Right Nothing
	parse _ = nopath

	nopath = return $ Left "Enter a location for the repository"

{- As well as checking the path for a lot of silly things, tilde is
 - expanded in the returned path. -}
checkRepositoryPath :: Text -> IO (Either (SomeMessage WebApp) (Maybe Text))
checkRepositoryPath p = do
	home <- myHomeDir
	let basepath = expandTilde home $ T.unpack p
	path <- absPath basepath
	let parent = parentDir path
	problems <- catMaybes <$> mapM runcheck
		[ (return $ path == "/", "Enter the full path to use for the repository.")
		, (return $ all isSpace basepath, "A blank path? Seems unlikely.")
		, (doesFileExist path, "A file already exists with that name.")
		, (return $ path == home, "Sorry, using git-annex for your whole home directory is not currently supported.")
		, (not <$> doesDirectoryExist parent, "Parent directory does not exist.")
		, (not <$> canWrite path, "Cannot write a repository there.")
		]
	return $ 
		case headMaybe problems of
			Nothing -> Right $ Just $ T.pack basepath
			Just prob -> Left prob
  where
	runcheck (chk, msg) = ifM (chk) ( return $ Just msg, return Nothing )
	expandTilde home ('~':'/':path) = home </> path
	expandTilde _ path = path

{- On first run, if run in the home directory, default to putting it in
 - ~/Desktop/annex, when a Desktop directory exists, and ~/annex otherwise.
 -
 - If run in another directory, that the user can write to,
 - the user probably wants to put it there. -}
defaultRepositoryPath :: Bool -> IO FilePath
defaultRepositoryPath firstrun = do
	cwd <- liftIO $ getCurrentDirectory
	home <- myHomeDir
	if home == cwd && firstrun
		then inhome
		else ifM (canWrite cwd) ( return cwd, inhome )
  where
	inhome = do
		desktop <- userDesktopDir
		ifM (doesDirectoryExist desktop)
			( relHome $ desktop </> gitAnnexAssistantDefaultDir
			, return $ "~" </> gitAnnexAssistantDefaultDir
			)

newRepositoryForm :: FilePath -> Form RepositoryPath
newRepositoryForm defpath msg = do
	(pathRes, pathView) <- mreq (repositoryPathField True) ""
		(Just $ T.pack $ addTrailingPathSeparator defpath)
	let (err, errmsg) = case pathRes of
		FormMissing -> (False, "")
		FormFailure l -> (True, concat $ map T.unpack l)
		FormSuccess _ -> (False, "")
	let form = do
		webAppFormAuthToken
		$(widgetFile "configurators/newrepository/form")
	return (RepositoryPath <$> pathRes, form)

{- Making the first repository, when starting the webapp for the first time. -}
getFirstRepositoryR :: Handler RepHtml
getFirstRepositoryR = page "Getting started" (Just Configuration) $ do
	path <- liftIO . defaultRepositoryPath =<< lift inFirstRun
	((res, form), enctype) <- lift $ runFormGet $ newRepositoryForm path
	case res of
		FormSuccess (RepositoryPath p) -> lift $
			startFullAssistant $ T.unpack p
		_ -> $(widgetFile "configurators/newrepository/first")

{- Adding a new local repository, which may be entirely separate, or may
 - be connected to the current repository. -}
getNewRepositoryR :: Handler RepHtml
getNewRepositoryR = page "Add another repository" (Just Configuration) $ do
	home <- liftIO myHomeDir
	((res, form), enctype) <- lift $ runFormGet $ newRepositoryForm home
	case res of
		FormSuccess (RepositoryPath p) -> do
			let path = T.unpack p
			liftIO $ makeRepo path False
			u <- liftIO $ initRepo True path Nothing
			lift $ runAnnex () $ setStandardGroup u ClientGroup
			liftIO $ addAutoStart path
			liftIO $ startAssistant path
			askcombine u path
		_ -> $(widgetFile "configurators/newrepository")
  where
	askcombine newrepouuid newrepopath = do
		newrepo <- liftIO $ relHome newrepopath
		mainrepo <- fromJust . relDir <$> lift getYesod
		$(widgetFile "configurators/newrepository/combine")

getCombineRepositoryR :: FilePathAndUUID -> Handler RepHtml
getCombineRepositoryR (FilePathAndUUID newrepopath newrepouuid) = do
	r <- combineRepos newrepopath remotename
	syncRemote r
	redirect $ EditRepositoryR newrepouuid
  where
	remotename = takeFileName newrepopath

data RemovableDrive = RemovableDrive 
	{ diskFree :: Maybe Integer
	, mountPoint :: Text
	}
	deriving (Show, Eq, Ord)

selectDriveForm :: [RemovableDrive] -> Maybe RemovableDrive -> Form RemovableDrive
selectDriveForm drives def = renderBootstrap $ RemovableDrive
	<$> pure Nothing
	<*> areq (selectFieldList pairs) "Select drive:" (mountPoint <$> def)
  where
	pairs = zip (map describe drives) (map mountPoint drives)
	describe drive = case diskFree drive of
		Nothing -> mountPoint drive
		Just free -> 
			let sz = roughSize storageUnits True free
			in T.unwords
				[ mountPoint drive
				, T.concat ["(", T.pack sz]
				, "free)"
				]

{- Adding a removable drive. -}
getAddDriveR :: Handler RepHtml
getAddDriveR = page "Add a removable drive" (Just Configuration) $ do
	removabledrives <- liftIO $ driveList
	writabledrives <- liftIO $
		filterM (canWrite . T.unpack . mountPoint) removabledrives
	((res, form), enctype) <- lift $ runFormGet $
		selectDriveForm (sort writabledrives) Nothing
	case res of
		FormSuccess (RemovableDrive { mountPoint = d }) -> lift $
			make (T.unpack d) >>= redirect . EditNewRepositoryR
		_ -> $(widgetFile "configurators/adddrive")
  where
	make mountpoint = do
		liftIO $ makerepo dir
		u <- liftIO $ initRepo False dir $ Just remotename
		r <- combineRepos dir remotename
		runAnnex () $ setStandardGroup u TransferGroup
		syncRemote r
		return u
	  where
		dir = mountpoint </> gitAnnexAssistantDefaultDir
		remotename = takeFileName mountpoint
	{- The repo may already exist, when adding removable media
	 - that has already been used elsewhere. -}
	makerepo dir = liftIO $ do
		r <- E.try (inDir dir $ getUUID) :: IO (Either E.SomeException UUID)
		case r of
			Right u | u /= NoUUID -> noop
			_ -> do
				createDirectoryIfMissing True dir
				makeRepo dir True

{- Each repository is made a remote of the other.
 - Next call syncRemote to get them in sync. -}
combineRepos :: FilePath -> String -> Handler Remote
combineRepos dir name = runAnnex undefined $ do
	hostname <- maybe "host" id <$> liftIO getHostname
	hostlocation <- fromRepo Git.repoLocation
	liftIO $ inDir dir $ void $ makeGitRemote hostname hostlocation
	addRemote $ makeGitRemote name dir

getEnableDirectoryR :: UUID -> Handler RepHtml
getEnableDirectoryR uuid = page "Enable a repository" (Just Configuration) $ do
	description <- lift $ runAnnex "" $
		T.pack . concat <$> prettyListUUIDs [uuid]
	$(widgetFile "configurators/enabledirectory")

{- List of removable drives. -}
driveList :: IO [RemovableDrive]
driveList = mapM (gen . mnt_dir) =<< filter sane <$> getMounts
  where
	gen dir = RemovableDrive
		<$> getDiskFree dir
		<*> pure (T.pack dir)
	-- filter out some things that are surely not removable drives
	sane Mntent { mnt_dir = dir, mnt_fsname = dev }
		{- We want real disks like /dev/foo, not
		 - dummy mount points like proc or tmpfs or
		 - gvfs-fuse-daemon. -}
		| not ('/' `elem` dev) = False
		{- Just in case: These mount points are surely not
		 - removable disks. -}
		| dir == "/" = False
		| dir == "/tmp" = False
		| dir == "/run/shm" = False
		| dir == "/run/lock" = False
		| otherwise = True

{- Bootstraps from first run mode to a fully running assistant in a
 - repository, by running the postFirstRun callback, which returns the
 - url to the new webapp. -}
startFullAssistant :: FilePath -> Handler ()
startFullAssistant path = do
	webapp <- getYesod
	url <- liftIO $ do
		makeRepo path False
		u <- initRepo True path Nothing
		inDir path $ 
			setStandardGroup u ClientGroup
		addAutoStart path
		changeWorkingDirectory path
		fromJust $ postFirstRun webapp
	redirect $ T.pack url

{- Makes a new git repository. -}
makeRepo :: FilePath -> Bool -> IO ()
makeRepo path bare = do
	(transcript, ok) <- processTranscript "git" (toCommand params) Nothing
	unless ok $
		error $ "git init failed!\nOutput:\n" ++ transcript
  where
	baseparams = [Param "init", Param "--quiet"]
	params
		| bare = baseparams ++ [Param "--bare", File path]
		| otherwise = baseparams ++ [File path]

{- Runs an action in the git-annex repository in the specified directory. -}
inDir :: FilePath -> Annex a -> IO a
inDir dir a = do
	state <- Annex.new =<< Git.Config.read =<< Git.Construct.fromPath dir
	Annex.eval state a

initRepo :: Bool -> FilePath -> Maybe String -> IO UUID
initRepo primary_assistant_repo dir desc = inDir dir $ do
	{- Initialize a git-annex repository in a directory with a description. -}
	unlessM isInitialized $
		initialize desc
	{- Initialize the master branch, so things that expect
	 - to have it will work, before any files are added. -}
	unlessM (Git.Config.isBare <$> gitRepo) $
		void $ inRepo $ Git.Command.runBool
			[ Param "commit"
			, Param "--quiet"
			, Param "--allow-empty"
			, Param "-m"
			, Param "created repository"
			]
	{- Repositories directly managed by the assistant use direct mode.
	 - 
	 - Automatic gc is disabled, as it can be slow. Insted, gc is done
	 - once a day.
	 -}
	when primary_assistant_repo $ do
		setDirect True
		inRepo $ Git.Command.run
			[Param "config", Param "gc.auto", Param "0"]
	getUUID

{- Adds a directory to the autostart file. -}
addAutoStart :: FilePath -> IO ()
addAutoStart path = do
	autostart <- autoStartFile
	createDirectoryIfMissing True (parentDir autostart)
	appendFile autostart $ path ++ "\n"

{- Checks if the user can write to a directory.
 -
 - The directory may be in the process of being created; if so
 - the parent directory is checked instead. -}
canWrite :: FilePath -> IO Bool		
canWrite dir = do
	tocheck <- ifM (doesDirectoryExist dir)
		(return dir, return $ parentDir dir)
	catchBoolIO $ fileAccess tocheck False True False
