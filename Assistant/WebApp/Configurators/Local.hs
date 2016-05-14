{- git-annex assistant webapp configurators for making local repositories
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, KindSignatures, TypeFamilies, FlexibleContexts #-}

module Assistant.WebApp.Configurators.Local where

import Assistant.WebApp.Common
import Assistant.WebApp.Gpg
import Assistant.WebApp.MakeRemote
import Assistant.Sync
import Assistant.Restart
import Annex.MakeRepo
import qualified Annex
import qualified Git
import qualified Git.Config
import qualified Git.Command
import qualified Command.Sync
import Config.Files
import Utility.FreeDesktop
import Utility.DiskFree
#ifndef mingw32_HOST_OS
import Utility.Mounts
#endif
import Utility.DataUnits
import Remote (prettyUUID)
import Annex.UUID
import Types.StandardGroups
import Logs.PreferredContent
import Logs.UUID
import Utility.UserInfo
import Config
import Utility.Gpg
import qualified Remote.GCrypt as GCrypt
import qualified Types.Remote

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char
import Data.Ord
import qualified Text.Hamlet as Hamlet

data RepositoryPath = RepositoryPath Text
	deriving Show

{- Custom field display for a RepositoryPath, with an icon etc.
 -
 - Validates that the path entered is not empty, and is a safe value
 - to use as a repository. -}
repositoryPathField :: forall (m :: * -> *). (MonadIO m, HandlerSite m ~ WebApp) => Bool -> Field m Text
repositoryPathField autofocus = Field
	{ fieldParse = \l _ -> parse l
	, fieldEnctype = UrlEncoded
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
	runcheck (chk, msg) = ifM chk ( return $ Just msg, return Nothing )
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
#ifndef mingw32_HOST_OS
	home <- myHomeDir
	currdir <- liftIO getCurrentDirectory
	if home == currdir && firstrun
		then inhome
		else ifM (legit currdir <&&> canWrite currdir)
			( return currdir
			, inhome
			)
#else
	-- On Windows, always default to ~/Desktop/annex or ~/annex,
	-- no cwd handling because the user might be able to write
	-- to the entire drive.
	if firstrun then inhome else inhome
#endif
  where
	inhome = do
		desktop <- userDesktopDir
		ifM (doesDirectoryExist desktop <&&> canWrite desktop)
			( relHome $ desktop </> gitAnnexAssistantDefaultDir
			, return $ "~" </> gitAnnexAssistantDefaultDir
			)
#ifndef mingw32_HOST_OS
	-- Avoid using eg, standalone build's git-annex.linux/ directory
	-- when run from there.
	legit d = not <$> doesFileExist (d </> "git-annex")
#endif

newRepositoryForm :: FilePath -> Hamlet.Html -> MkMForm RepositoryPath
newRepositoryForm defpath msg = do
	(pathRes, pathView) <- mreq (repositoryPathField True) (bfs "")
		(Just $ T.pack $ addTrailingPathSeparator defpath)
	let (err, errmsg) = case pathRes of
		FormMissing -> (False, "")
		FormFailure l -> (True, concatMap T.unpack l)
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
	unlessM (liftIO $ inPath "git") $
		error "You need to install git in order to use git-annex!"
#ifdef __ANDROID__
	androidspecial <- liftIO $ doesDirectoryExist "/sdcard/DCIM"
	let path = "/sdcard/annex"
#else
	let androidspecial = False
	path <- liftIO . defaultRepositoryPath =<< liftH inFirstRun
#endif
	((res, form), enctype) <- liftH $ runFormPostNoToken $ newRepositoryForm path
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
			writeFile ".gitignore" ".thumbnails"
		void $ inRepo $
			Git.Command.runBool [Param "add", File ".gitignore"]

{- Adding a new local repository, which may be entirely separate, or may
 - be connected to the current repository. -}
getNewRepositoryR :: Handler Html
getNewRepositoryR = postNewRepositoryR
postNewRepositoryR :: Handler Html
postNewRepositoryR = page "Add another repository" (Just Configuration) $ do
	home <- liftIO myHomeDir
	((res, form), enctype) <- liftH $ runFormPostNoToken $ newRepositoryForm home
	case res of
		FormSuccess (RepositoryPath p) -> do
			let path = T.unpack p
			isnew <- liftIO $ makeRepo path False
			u <- liftIO $ initRepo isnew True path Nothing (Just ClientGroup)
			liftIO $ addAutoStartFile path
			liftIO $ startAssistant path
			askcombine u path
		_ -> $(widgetFile "configurators/newrepository")
  where
	askcombine newrepouuid newrepopath = do
		newrepo <- liftIO $ relHome newrepopath
		mainrepo <- fromJust . relDir <$> liftH getYesod
		$(widgetFile "configurators/newrepository/combine")

{- Ensure that a remote's description, group, etc are available by
 - immediately pulling from it. Also spawns a sync to push to it as well. -}
immediateSyncRemote :: Remote -> Assistant ()
immediateSyncRemote r = do
	currentbranch <- liftAnnex $ join Command.Sync.getCurrBranch
	void $ manualPull currentbranch [r]
	syncRemote r

getCombineRepositoryR :: FilePath -> UUID -> Handler Html
getCombineRepositoryR newrepopath newrepouuid = do
	liftAssistant . immediateSyncRemote =<< combineRepos newrepopath remotename
	redirect $ EditRepositoryR $ RepoUUID newrepouuid
  where
	remotename = takeFileName newrepopath

selectDriveForm :: [RemovableDrive] -> Hamlet.Html -> MkMForm RemovableDrive
selectDriveForm drives = renderBootstrap3 bootstrapFormLayout $ RemovableDrive
	<$> pure Nothing
	<*> areq (selectFieldList pairs `withNote` onlywritable) (bfs "Select drive:") Nothing
	<*> areq textField (bfs "Use this directory on the drive:")
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
	onlywritable = [whamlet|This list only includes drives you can write to.|]

removableDriveRepository :: RemovableDrive -> FilePath
removableDriveRepository drive =
	T.unpack (mountPoint drive) </> T.unpack (driveRepoPath drive)

{- Adding a removable drive. -}
getAddDriveR :: Handler Html
getAddDriveR = postAddDriveR
postAddDriveR :: Handler Html
postAddDriveR = page "Add a removable drive" (Just Configuration) $ do
	removabledrives <- liftIO driveList
	writabledrives <- liftIO $
		filterM (canWrite . T.unpack . mountPoint) removabledrives
	((res, form), enctype) <- liftH $ runFormPostNoToken $
		selectDriveForm (sort writabledrives)
	case res of
		FormSuccess drive -> liftH $ redirect $ ConfirmAddDriveR drive
		_ -> $(widgetFile "configurators/adddrive")

{- The repo may already exist, when adding removable media
 - that has already been used elsewhere. If so, check
 - the UUID of the repo and see if it's one we know. If not,
 - the user must confirm the repository merge.
 -
 - If the repo does not already exist on the drive, prompt about
 - encryption. -}
getConfirmAddDriveR :: RemovableDrive -> Handler Html
getConfirmAddDriveR drive = ifM (liftIO $ probeRepoExists dir)
	( do
		mu <- liftIO $ probeUUID dir
		case mu of
			Nothing -> maybe askcombine isknownuuid
				=<< liftAnnex (probeGCryptRemoteUUID dir)
			Just driveuuid -> isknownuuid driveuuid
	, newrepo
	)
  where
	dir = removableDriveRepository drive
	newrepo = do
		cmd <- liftAnnex $ gpgCmd <$> Annex.getGitConfig
		secretkeys <- sortBy (comparing snd) . M.toList
			<$> liftIO (secretKeys cmd)
		page "Encrypt repository?" (Just Configuration) $
			$(widgetFile "configurators/adddrive/encrypt")
	knownrepo = getFinishAddDriveR drive NoRepoKey
	askcombine = page "Combine repositories?" (Just Configuration) $
		$(widgetFile "configurators/adddrive/combine")
	isknownuuid driveuuid =
		ifM (M.member driveuuid <$> liftAnnex uuidMap)
			( knownrepo
			, askcombine
			)

setupDriveModal :: Widget
setupDriveModal = $(widgetFile "configurators/adddrive/setupmodal")

getGenKeyForDriveR :: RemovableDrive -> Handler Html
getGenKeyForDriveR drive = withNewSecretKey $ \keyid ->
	{- Generating a key takes a long time, and 
	 - the removable drive may have been disconnected
	 - in the meantime. Check that it is still mounted
	 - before finishing. -}
	ifM (liftIO $ any (\d -> mountPoint d == mountPoint drive) <$> driveList)
		( getFinishAddDriveR drive (RepoKey keyid)
		, getAddDriveR
		)

getFinishAddDriveR :: RemovableDrive -> RepoKey -> Handler Html
getFinishAddDriveR drive = go
  where
	go (RepoKey keyid) = whenGcryptInstalled $ makewith $ const $ do
		r <- liftAnnex $ addRemote $
			makeGCryptRemote remotename dir keyid
		return (Types.Remote.uuid r, r)
	go NoRepoKey = checkGCryptRepoEncryption dir makeunencrypted makeunencrypted $ do
		mu <- liftAnnex $ probeGCryptRemoteUUID dir
		case mu of
			Just u -> enableexistinggcryptremote u
			Nothing -> error "The drive contains a gcrypt repository that is not a git-annex special remote. This is not supported."
	enableexistinggcryptremote u = do
		remotename' <- liftAnnex $ getGCryptRemoteName u dir
		makewith $ const $ do
			r <- liftAnnex $ addRemote $
				enableSpecialRemote remotename' GCrypt.remote Nothing $ M.fromList
					[("gitrepo", dir)]
			return (u, r)
	{- Making a new unencrypted repo, or combining with an existing one. -}
	makeunencrypted = makewith $ \isnew -> (,)
		<$> liftIO (initRepo isnew False dir (Just remotename) Nothing)
		<*> combineRepos dir remotename
	makewith a = do
		liftIO $ createDirectoryIfMissing True dir
		isnew <- liftIO $ makeRepo dir True
		{- Removable drives are not reliable media, so enable fsync. -}
		liftIO $ inDir dir $
			setConfig (ConfigKey "core.fsyncobjectfiles")
				(Git.Config.boolConfig True)
		(u, r) <- a isnew
		when isnew $
			liftAnnex $ defaultStandardGroup u TransferGroup
		liftAssistant $ immediateSyncRemote r
		redirect $ EditNewRepositoryR u
	mountpoint = T.unpack (mountPoint drive)
	dir = removableDriveRepository drive
	remotename = takeFileName mountpoint

{- Each repository is made a remote of the other.
 - Next call syncRemote to get them in sync. -}
combineRepos :: FilePath -> String -> Handler Remote
combineRepos dir name = liftAnnex $ do
	hostname <- fromMaybe "host" <$> liftIO getHostname
	mylocation <- fromRepo Git.repoLocation
	mypath <- liftIO $ relPathDirToFile dir mylocation
	liftIO $ inDir dir $ void $ makeGitRemote hostname mypath
	addRemote $ makeGitRemote name dir

getEnableDirectoryR :: UUID -> Handler Html
getEnableDirectoryR uuid = page "Enable a repository" (Just Configuration) $ do
	description <- liftAnnex $ T.pack <$> prettyUUID uuid
	$(widgetFile "configurators/enabledirectory")

{- List of removable drives. -}
driveList :: IO [RemovableDrive]
#ifdef mingw32_HOST_OS
-- Just enumerate all likely drive letters for Windows.
-- Could use wmic, but it only works for administrators.
driveList = mapM (\d -> genRemovableDrive $ d:":\\") ['A'..'Z']
#else
driveList = mapM (genRemovableDrive . mnt_dir) =<< filter sane <$> getMounts
  where
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
#endif

genRemovableDrive :: FilePath -> IO RemovableDrive
genRemovableDrive dir = RemovableDrive
	<$> getDiskFree dir
	<*> pure (T.pack dir)
	<*> pure (T.pack gitAnnexAssistantDefaultDir)

{- Bootstraps from first run mode to a fully running assistant in a
 - repository, by running the postFirstRun callback, which returns the
 - url to the new webapp. -}
startFullAssistant :: FilePath -> StandardGroup -> Maybe (Annex ())-> Handler ()
startFullAssistant path repogroup setup = do
	webapp <- getYesod
	url <- liftIO $ do
		isnew <- makeRepo path False
		void $ initRepo isnew True path Nothing (Just repogroup)
		inDir path $ fromMaybe noop setup
		addAutoStartFile path
		setCurrentDirectory path
		fromJust $ postFirstRun webapp
	redirect $ T.pack url

{- Checks if the user can write to a directory.
 -
 - The directory may be in the process of being created; if so
 - the parent directory is checked instead. -}
canWrite :: FilePath -> IO Bool		
canWrite dir = do
	tocheck <- ifM (doesDirectoryExist dir)
		(return dir, return $ parentDir dir)
	catchBoolIO $ fileAccess tocheck False True False

{- Gets the UUID of the git repo at a location, which may not exist, or
 - not be a git-annex repo. -}
probeUUID :: FilePath -> IO (Maybe UUID)
probeUUID dir = catchDefaultIO Nothing $ inDir dir $ do
	u <- getUUID
	return $ if u == NoUUID then Nothing else Just u
