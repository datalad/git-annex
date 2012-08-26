{- git-annex assistant webapp configurators
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.SideBar
import Assistant.DaemonStatus
import Assistant.Threads.MountWatcher (handleMount)
import Utility.Yesod
import qualified Remote
import qualified Types.Remote as Remote
import Remote.List
import Annex.UUID (getUUID)
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

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import System.Posix.Directory
import qualified Control.Exception as E

{- The main configuration screen. -}
getConfigR :: Handler RepHtml
getConfigR = ifM (inFirstRun)
	( getFirstRepositoryR
	, bootstrap (Just Config) $ do
		sideBarDisplay
		setTitle "Configuration"
		$(widgetFile "configurators/main")
	)

{- Lists known repositories, followed by options to add more. -}
getRepositoriesR :: Handler RepHtml
getRepositoriesR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Repositories"
	repolist <- lift repoList
	$(widgetFile "configurators/repositories")

{- A numbered list of known repositories, including the current one. -}
repoList :: Handler [(String, String)]
repoList = do
	rs <- filter (not . Remote.readonly) . knownRemotes <$>
		(liftIO . getDaemonStatus =<< daemonStatus <$> getYesod)
	l <- runAnnex [] $ do
		u <- getUUID
		Remote.prettyListUUIDs $ nub $ u:(map Remote.uuid rs)
	return $ zip counter l
	where
		counter = map show ([1..] :: [Int])

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	repolist <- lift repoList
	let n = length repolist
	let numrepos = show n
	let notenough = n < enough
	let barelyenough = n == enough
	let morethanenough = n > enough
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }
	where
		enough = 2

data RepositoryPath = RepositoryPath Text
	deriving Show

{- Custom field display for a RepositoryPath, with an icon etc.
 -
 - Validates that the path entered is not empty, and is a safe value
 - to use as a repository. -}
repositoryPathField :: forall sub. Bool -> Field sub WebApp Text
repositoryPathField autofocus = Field { fieldParse = parse, fieldView = view }
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
		, (not <$> canMakeSymlink path, "That directory is on a filesystem that does not support symlinks. Try a different location.")
		]
	return $ 
		case headMaybe problems of
			Nothing -> Right $ Just $ T.pack basepath
			Just prob -> Left prob
	where
		runcheck (chk, msg) = ifM (chk)
			( return $ Just msg
			, return Nothing
			)
		expandTilde home ('~':'/':path) = home </> path
		expandTilde _ path = path
		

{- On first run, if run in the home directory, default to putting it in
 - ~/Desktop/annex, when a Desktop directory exists, and ~/annex otherwise.
 -
 - If run in another directory, the user probably wants to put it there. -}
defaultRepositoryPath :: Bool -> IO FilePath
defaultRepositoryPath firstrun = do
	cwd <- liftIO $ getCurrentDirectory
	home <- myHomeDir
	if home == cwd && firstrun
		then do
			desktop <- userDesktopDir
			ifM (doesDirectoryExist desktop)
				(relHome (desktop </> "annex"), return "~/annex")
		else return cwd

localRepositoryForm :: Form RepositoryPath
localRepositoryForm msg = do
	path <- T.pack . addTrailingPathSeparator
		<$> (liftIO . defaultRepositoryPath =<< lift inFirstRun)
	(pathRes, pathView) <- mreq (repositoryPathField True) "" (Just path)
	let (err, errmsg) = case pathRes of
		FormMissing -> (False, "")
		FormFailure l -> (True, concat $ map T.unpack l)
		FormSuccess _ -> (False, "")
	let form = do
		webAppFormAuthToken
		$(widgetFile "configurators/localrepositoryform")
	return (RepositoryPath <$> pathRes, form)

{- Making the first repository, when starting the webapp for the first time. -}
getFirstRepositoryR :: Handler RepHtml
getFirstRepositoryR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Getting started"
	((res, form), enctype) <- lift $ runFormGet localRepositoryForm
	case res of
		FormSuccess (RepositoryPath p) -> lift $
			startFullAssistant $ T.unpack p
		_ -> $(widgetFile "configurators/firstrepository")

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
getAddDriveR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add a removable drive"
	removabledrives <- liftIO $ driveList
	writabledrives <- liftIO $
		filterM (canWrite . T.unpack . mountPoint) removabledrives
	((res, form), enctype) <- lift $ runFormGet $
		selectDriveForm (sort writabledrives) Nothing
	case res of
		FormSuccess (RemovableDrive { mountPoint = d }) -> lift $ do
			go $ T.unpack d
			setMessage $ toHtml $ T.unwords ["Added", d]
			redirect RepositoriesR
		_ -> do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/adddrive")
	where
		go mountpoint = do
			liftIO $ makerepo dir
			liftIO $ initRepo dir $ Just remotename
			addremotes dir remotename
			webapp <- getYesod
			liftIO $ syncrepo dir webapp
			where
				dir = mountpoint </> "annex"
				remotename = takeFileName mountpoint
		{- The repo may already exist, when adding removable media
		 - that has already been used elsewhere. -}
		makerepo dir = liftIO $ do
			r <- E.try (inDir dir $ return True) :: IO (Either E.SomeException Bool)
			case r of
				Right _ -> noop
				Left _e -> do
					createDirectoryIfMissing True dir
					bare <- not <$> canMakeSymlink dir
					makeRepo dir bare
		{- Synthesize a mount event of the new git repository.
		 - This will sync it, and queue file transfers. -}
		syncrepo dir webapp =
			handleMount
				(fromJust $ threadState webapp)
				(daemonStatus webapp)
				(scanRemotes webapp)
				dir
		{- Each repository is made a remote of the other. -}
		addremotes dir name = runAnnex () $ do
			hostname <- maybe "host" id <$> liftIO getHostname
			hostlocation <- fromRepo Git.repoLocation
			void $ liftIO $ inDir dir $
				addremote hostname hostlocation
			whenM (addremote name dir) $
				void $ remoteListRefresh
		{- Adds a remote only if there is not already one with
		 - the location. -}
		addremote name location = inRepo $ \r ->
			if (null $ filter samelocation $ Git.remotes r)
				then do
					let name' = uniqueremotename r name (0 :: Int)
					Git.Command.runBool "remote"
						[Param "add", Param name', Param location] r
				else return False
			where
				samelocation x = Git.repoLocation x == location
		{- Generate an unused name for a remote, adding a number if
		 - necessary. -}
		uniqueremotename r basename n
			| null namecollision = name
			| otherwise = uniqueremotename r basename (succ n)
			where
				namecollision = filter samename (Git.remotes r)
				samename x = Git.remoteName x == Just name
				name
					| n == 0 = basename
					| otherwise = basename ++ show n

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
		initRepo path Nothing
		addAutoStart path
		changeWorkingDirectory path
		fromJust $ postFirstRun webapp
	redirect $ T.pack url

{- Makes a new git-annex repository. -}
makeRepo :: FilePath -> Bool -> IO ()
makeRepo path bare = do
	unlessM (boolSystem "git" params) $
		error "git init failed!"
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

{- Initializes a git-annex repository in a directory with a description. -}
initRepo :: FilePath -> Maybe String -> IO ()
initRepo dir desc = inDir dir $
	unlessM isInitialized $
		initialize desc

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

{- Checks if a directory is on a filesystem that supports symlinks. -}
canMakeSymlink :: FilePath -> IO Bool
canMakeSymlink dir = ifM (doesDirectoryExist dir)
	( catchBoolIO $ test dir
	, canMakeSymlink (parentDir dir)
	)
	where
		test d = do
			let link = d </> "delete.me"
			createSymbolicLink link link
			removeLink link
			return True
