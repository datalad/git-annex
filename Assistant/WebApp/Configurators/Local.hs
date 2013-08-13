{- git-annex assistant webapp configurators for making local repositories
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, OverloadedStrings, RankNTypes, KindSignatures, TypeFamilies #-}

module Assistant.WebApp.Configurators.Local where

import Assistant.WebApp.Common
import Assistant.WebApp.OtherRepos
import Assistant.MakeRemote
import Assistant.Sync
import Init
import qualified Git
import qualified Git.Construct
import qualified Git.Config
import qualified Git.Command
import qualified Annex
import Config.Files
import Utility.FreeDesktop
#ifdef WITH_CLIBS
import Utility.Mounts
#endif
import Utility.DiskFree
import Utility.DataUnits
import Utility.Network
import Remote (prettyUUID)
import Annex.UUID
import Types.StandardGroups
import Logs.PreferredContent
import Logs.UUID
import Utility.UserInfo
import Config

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char
import qualified Text.Hamlet as Hamlet

data RepositoryPath = RepositoryPath Text
	deriving Show

{- Custom field display for a RepositoryPath, with an icon etc.
 -
 - Validates that the path entered is not empty, and is a safe value
 - to use as a repository. -}
#if MIN_VERSION_yesod(1,2,0)
repositoryPathField :: forall (m :: * -> *). (MonadIO m, HandlerSite m ~ WebApp) => Bool -> Field m Text
#else
repositoryPathField :: forall sub. Bool -> Field sub WebApp Text
#endif
repositoryPathField autofocus = Field
#if ! MIN_VERSION_yesod_form(1,2,0)
	{ fieldParse = parse
#else
	{ fieldParse = \l _ -> parse l
	, fieldEnctype = UrlEncoded
#endif
	, fieldView = view
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
 - the user probably wants to put it there. Unless that directory
 - contains a git-annex file, in which case the user has probably
 - browsed to a directory with git-annex and run it from there. -}
defaultRepositoryPath :: Bool -> IO FilePath
defaultRepositoryPath firstrun = do
	cwd <- liftIO $ getCurrentDirectory
	home <- myHomeDir
	if home == cwd && firstrun
		then inhome
		else ifM (legit cwd <&&> canWrite cwd)
			( return cwd
			, inhome
			)
  where
	inhome = do
		desktop <- userDesktopDir
		ifM (doesDirectoryExist desktop)
			( relHome $ desktop </> gitAnnexAssistantDefaultDir
			, return $ "~" </> gitAnnexAssistantDefaultDir
			)
	legit d = not <$> doesFileExist (d </> "git-annex")

newRepositoryForm :: FilePath -> Hamlet.Html -> MkMForm RepositoryPath
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
getFirstRepositoryR :: Handler Html
getFirstRepositoryR = postFirstRepositoryR
postFirstRepositoryR :: Handler Html
postFirstRepositoryR = page "Getting started" (Just Configuration) $ do
#ifdef __ANDROID__
	androidspecial <- liftIO $ doesDirectoryExist "/sdcard/DCIM"
	let path = "/sdcard/annex"
#else
	let androidspecial = False
	path <- liftIO . defaultRepositoryPath =<< liftH inFirstRun
#endif
	((res, form), enctype) <- liftH $ runFormPost $ newRepositoryForm path
	case res of
		FormSuccess (RepositoryPath p) -> liftH $
			startFullAssistant (T.unpack p) ClientGroup Nothing
		_ -> $(widgetFile "configurators/newrepository/first")

getAndroidCameraRepositoryR :: Handler ()
getAndroidCameraRepositoryR = 
	startFullAssistant "/sdcard/DCIM" SourceGroup $ Just addignore	
  where
  	addignore = do
		liftIO $ unlessM (doesFileExist ".gitignore") $
			writeFile ".gitignore" ".thumbnails/*"
		void $ inRepo $
			Git.Command.runBool [Param "add", File ".gitignore"]

{- Adding a new local repository, which may be entirely separate, or may
 - be connected to the current repository. -}
getNewRepositoryR :: Handler Html
getNewRepositoryR = postNewRepositoryR
postNewRepositoryR :: Handler Html
postNewRepositoryR = page "Add another repository" (Just Configuration) $ do
	home <- liftIO myHomeDir
	((res, form), enctype) <- liftH $ runFormPost $ newRepositoryForm home
	case res of
		FormSuccess (RepositoryPath p) -> do
			let path = T.unpack p
			isnew <- liftIO $ makeRepo path False
			u <- liftIO $ initRepo isnew True path Nothing
			liftH $ liftAnnexOr () $ setStandardGroup u ClientGroup
			liftIO $ addAutoStartFile path
			liftIO $ startAssistant path
			askcombine u path
		_ -> $(widgetFile "configurators/newrepository")
  where
	askcombine newrepouuid newrepopath = do
		newrepo <- liftIO $ relHome newrepopath
		mainrepo <- fromJust . relDir <$> liftH getYesod
		$(widgetFile "configurators/newrepository/combine")

getCombineRepositoryR :: FilePathAndUUID -> Handler Html
getCombineRepositoryR (FilePathAndUUID newrepopath newrepouuid) = do
	r <- combineRepos newrepopath remotename
	liftAssistant $ syncRemote r
	redirect $ EditRepositoryR newrepouuid
  where
	remotename = takeFileName newrepopath

selectDriveForm :: [RemovableDrive] -> Hamlet.Html -> MkMForm RemovableDrive
selectDriveForm drives = renderBootstrap $ RemovableDrive
	<$> pure Nothing
	<*> areq (selectFieldList pairs) "Select drive:" Nothing
	<*> areq textField "Use this directory on the drive:"
		(Just $ T.pack gitAnnexAssistantDefaultDir)
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

removableDriveRepository :: RemovableDrive -> FilePath
removableDriveRepository drive =
	T.unpack (mountPoint drive) </> T.unpack (driveRepoPath drive)

{- Adding a removable drive. -}
getAddDriveR :: Handler Html
getAddDriveR = postAddDriveR
postAddDriveR :: Handler Html
postAddDriveR = page "Add a removable drive" (Just Configuration) $ do
	removabledrives <- liftIO $ driveList
	writabledrives <- liftIO $
		filterM (canWrite . T.unpack . mountPoint) removabledrives
	((res, form), enctype) <- liftH $ runFormPost $
		selectDriveForm (sort writabledrives)
	case res of
		FormSuccess drive -> liftH $ redirect $ ConfirmAddDriveR drive
		_ -> $(widgetFile "configurators/adddrive")

{- The repo may already exist, when adding removable media
 - that has already been used elsewhere. If so, check
 - the UUID of the repo and see if it's one we know. If not,
 - the user must confirm the repository merge. -}
getConfirmAddDriveR :: RemovableDrive -> Handler Html
getConfirmAddDriveR drive = do
	ifM (needconfirm)
		( page "Combine repositories?" (Just Configuration) $
			$(widgetFile "configurators/adddrive/confirm")
		, do
			getFinishAddDriveR drive
		)
  where
  	dir = removableDriveRepository drive
	needconfirm = ifM (liftIO $ doesDirectoryExist dir)
		( liftAnnex $ do
			mu <- liftIO $ catchMaybeIO $
				inDir dir $ getUUID
			case mu of
				Nothing -> return False
				Just driveuuid -> not .
					M.member driveuuid <$> uuidMap
		, return False
		)

cloneModal :: Widget
cloneModal = $(widgetFile "configurators/adddrive/clonemodal")

getFinishAddDriveR :: RemovableDrive -> Handler Html
getFinishAddDriveR drive = make >>= redirect . EditNewRepositoryR
  where
  	make = do
		liftIO $ createDirectoryIfMissing True dir
		isnew <- liftIO $ makeRepo dir True
		u <- liftIO $ initRepo isnew False dir $ Just remotename
		{- Removable drives are not reliable media, so enable fsync. -}
		liftIO $ inDir dir $
			setConfig (ConfigKey "core.fsyncobjectfiles")
				(Git.Config.boolConfig True)
		r <- combineRepos dir remotename
		liftAnnex $ setStandardGroup u TransferGroup
		liftAssistant $ syncRemote r
		return u
  	mountpoint = T.unpack (mountPoint drive)
	dir = removableDriveRepository drive
	remotename = takeFileName mountpoint

{- Each repository is made a remote of the other.
 - Next call syncRemote to get them in sync. -}
combineRepos :: FilePath -> String -> Handler Remote
combineRepos dir name = liftAnnex $ do
	hostname <- maybe "host" id <$> liftIO getHostname
	hostlocation <- fromRepo Git.repoLocation
	liftIO $ inDir dir $ void $ makeGitRemote hostname hostlocation
	addRemote $ makeGitRemote name dir

getEnableDirectoryR :: UUID -> Handler Html
getEnableDirectoryR uuid = page "Enable a repository" (Just Configuration) $ do
	description <- liftAnnex $ T.pack <$> prettyUUID uuid
	$(widgetFile "configurators/enabledirectory")

{- List of removable drives. -}
driveList :: IO [RemovableDrive]
#ifdef WITH_CLIBS
driveList = mapM (gen . mnt_dir) =<< filter sane <$> getMounts
  where
	gen dir = RemovableDrive
		<$> getDiskFree dir
		<*> pure (T.pack dir)
		<*> pure (T.pack gitAnnexAssistantDefaultDir)
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
#ifdef __ANDROID__
		| dir == "/mnt/sdcard" = False
		| dir == "/sdcard" = False
#endif
		| otherwise = True
#else
driveList = return []
#endif

{- Bootstraps from first run mode to a fully running assistant in a
 - repository, by running the postFirstRun callback, which returns the
 - url to the new webapp. -}
startFullAssistant :: FilePath -> StandardGroup -> Maybe (Annex ())-> Handler ()
startFullAssistant path repogroup setup = do
	webapp <- getYesod
	url <- liftIO $ do
		isnew <- makeRepo path False
		u <- initRepo isnew True path Nothing
		inDir path $ do
			setStandardGroup u repogroup
			maybe noop id setup
		addAutoStartFile path
		setCurrentDirectory path
		fromJust $ postFirstRun webapp
	redirect $ T.pack url

{- Makes a new git repository. Or, if a git repository already
 - exists, returns False. -}
makeRepo :: FilePath -> Bool -> IO Bool
makeRepo path bare = ifM alreadyexists
	( return False
	, do
		(transcript, ok) <-
			processTranscript "git" (toCommand params) Nothing
		unless ok $
			error $ "git init failed!\nOutput:\n" ++ transcript
		return True
	)
  where
  	alreadyexists = isJust <$> 
		catchDefaultIO Nothing (Git.Construct.checkForRepo path)
	baseparams = [Param "init", Param "--quiet"]
	params
		| bare = baseparams ++ [Param "--bare", File path]
		| otherwise = baseparams ++ [File path]

{- Runs an action in the git-annex repository in the specified directory. -}
inDir :: FilePath -> Annex a -> IO a
inDir dir a = do
	state <- Annex.new =<< Git.Config.read =<< Git.Construct.fromPath dir
	Annex.eval state a

{- Creates a new repository, and returns its UUID. -}
initRepo :: Bool -> Bool -> FilePath -> Maybe String -> IO UUID
initRepo True primary_assistant_repo dir desc = inDir dir $ do
	initRepo' desc
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
{- Repo already exists, could be a non-git-annex repo though. -}
initRepo False _ dir desc = inDir dir $ do
	initRepo' desc
	getUUID

initRepo' :: Maybe String -> Annex ()
initRepo' desc = do
	unlessM isInitialized $
		initialize desc

{- Checks if the user can write to a directory.
 -
 - The directory may be in the process of being created; if so
 - the parent directory is checked instead. -}
canWrite :: FilePath -> IO Bool		
canWrite dir = do
	tocheck <- ifM (doesDirectoryExist dir)
		(return dir, return $ parentDir dir)
	catchBoolIO $ fileAccess tocheck False True False
