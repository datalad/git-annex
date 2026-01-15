{- git-annex repository initialization
 -
 - Copyright 2011-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Annex.Init (
	checkInitializeAllowed,
	ensureInitialized,
	autoInitialize,
	autoInitialize',
	isInitialized,
	initialize,
	initialize',
	uninitialize,
	probeCrippledFileSystem,
	probeCrippledFileSystem',
	isCrippledFileSystem,
	propigateInitGitConfigs,
) where

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.Config
import qualified Git.Objects
import Git.Types (fromConfigValue)
import Git.ConfigTypes (SharedRepository(..))
import qualified Annex.Branch
import qualified Database.Fsck
import Logs.UUID
import Logs.Trust.Basic
import Logs.Config
import Logs.PreferredContent.Raw
import Logs.Group
import Types.TrustLevel
import Types.RepoVersion
import Annex.Version
import Annex.Difference
import Annex.UUID
import Annex.Fixup
import Annex.Path
import Config
import Config.Files
import Config.Smudge
import qualified Upgrade.V5.Direct as Direct
import qualified Annex.AdjustedBranch as AdjustedBranch
import Remote.List.Util (remotesChanged)
import Annex.Environment
import Annex.Hook
import Annex.InodeSentinal
import Upgrade
import Annex.Tmp
import Utility.UserInfo
import Annex.Perms
#ifndef mingw32_HOST_OS
import Utility.ThreadScheduler
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F
import Utility.FileMode
import System.Posix.User
import qualified Utility.LockFile.Posix as Posix
#endif

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.IO.Class (MonadIO)
#ifndef mingw32_HOST_OS
import System.PosixCompat.Files (ownerReadMode, isNamedPipe)
import Data.Either
import Control.Concurrent.Async
#endif

data InitializeAllowed = InitializeAllowed

checkInitializeAllowed :: (InitializeAllowed -> Annex a) -> Annex a
checkInitializeAllowed a = guardSafeToUseRepo $ noAnnexFileContent' >>= \case
	Nothing -> runAnnexHook' preInitAnnexHook annexPreInitCommand >>= \case
		HookSuccess -> do
			checkSqliteWorks
			a InitializeAllowed
		HookFailed failedcommanddesc -> do
			initpreventedby failedcommanddesc
			notinitialized
	Just noannexmsg -> do
		initpreventedby ".noannex file (remove the file to override)"
		unless (null noannexmsg) $
			warning (UnquotedString noannexmsg)
		notinitialized
  where
	initpreventedby r = warning $ UnquotedString $
		"Initialization prevented by " ++ r
	notinitialized = giveup "Not initialized."

initializeAllowed :: Annex Bool
initializeAllowed = noAnnexFileContent' >>= \case
	Nothing -> runAnnexHook' preInitAnnexHook annexPreInitCommand >>= \case
		HookSuccess -> return True
		HookFailed _ -> return False
	Just _ -> return False

noAnnexFileContent' :: Annex (Maybe String)
noAnnexFileContent' = inRepo $ noAnnexFileContent . Git.repoWorkTree

genDescription :: Maybe String -> Annex UUIDDesc
genDescription (Just d) = return $ UUIDDesc $ encodeBS d
genDescription Nothing = do
	reldir <- liftIO . relHome
		=<< liftIO . absPath
		=<< fromRepo Git.repoPath
	hostname <- fromMaybe "" <$> liftIO getHostname
	let at = if null hostname then "" else "@"
	v <- liftIO myUserName
	return $ UUIDDesc $ encodeBS $ concat $ case v of
		Right username -> [username, at, hostname, ":", fromOsPath reldir]
		Left _ -> [hostname, ":", fromOsPath reldir]

initialize :: Annex () -> Maybe String -> Maybe RepoVersion -> Annex ()
initialize startupannex mdescription mversion = checkInitializeAllowed $ \initallowed -> do
	{- Has to come before any commits are made as the shared
	 - clone heuristic expects no local objects. -}
	sharedclone <- checkSharedClone

	{- This will make the first commit to git, so ensure git is set up
	 - properly to allow commits when running it. -}
	ensureCommit $ Annex.Branch.create

	prepUUID
	initialize' startupannex mversion initallowed
	
	initSharedClone sharedclone
	
	u <- getUUID
	when (u == NoUUID) $
		giveup "Failed to read annex.uuid from git config after setting it. This should never happen. Please file a bug report."

	{- Avoid overwriting existing description with a default
	 - description. -}
	whenM (pure (isJust mdescription) <||> not . M.member u <$> uuidDescMapRaw) $
		describeUUID u =<< genDescription mdescription

-- Everything except for uuid setup, shared clone setup, and initial
-- description.
initialize' :: Annex () -> Maybe RepoVersion -> InitializeAllowed -> Annex ()
initialize' startupannex mversion _initallowed = do
	checkLockSupport
	checkFifoSupport
	checkCrippledFileSystem
	unlessM isBareRepo $ do
		hookWrite preCommitHook
		hookWrite postReceiveHook
	setDifferences
	initialversion <- getVersion
	unless (isJust initialversion) $
		setVersion (fromMaybe defaultVersion mversion)
	supportunlocked <- annexSupportUnlocked <$> Annex.getGitConfig
	if supportunlocked
		then configureSmudgeFilter
		else deconfigureSmudgeFilter
	unlessM isBareRepo $ do
		hookWrite postCheckoutHook
		hookWrite postMergeHook

	AdjustedBranch.checkAdjustedClone >>= \case
		AdjustedBranch.InAdjustedClone -> return ()
		AdjustedBranch.NotInAdjustedClone ->
			ifM (crippledFileSystem <&&> (not <$> isBareRepo))
				( AdjustedBranch.adjustToCrippledFileSystem
				-- Handle case where this repo was cloned from a
				-- direct mode repo
				, unlessM isBareRepo
					Direct.switchHEADBack
				)
	propigateSecureHashesOnly
	when (isNothing initialversion) $
		propigateInitGitConfigs =<< getUUID
	createInodeSentinalFile False
	fixupUnusualReposAfterInit

	-- This is usually run at Annex startup, but when git-annex was
	-- not already initialized, it will not yet have run.
	startupannex

uninitialize :: Annex ()
uninitialize = do
	-- Remove hooks that are written when initializing.
	hookUnWrite preCommitHook
	hookUnWrite postReceiveHook
	hookUnWrite postCheckoutHook
	hookUnWrite postMergeHook
	deconfigureSmudgeFilter
	removeRepoUUID
	removeVersion

{- Gets the version that the repo is initialized with.
 -
 - To make sure the repo is fully initialized, also checks that it has a
 - uuid configured. In the unusual case where one is set and the other is
 - not, errors out to avoid running in an inconsistent state.
 -}
getInitializedVersion :: Annex (Maybe RepoVersion)
getInitializedVersion = do
	um <- (\u -> if u == NoUUID then Nothing else Just u) <$> getUUID
	vm <- getVersion
	case (um, vm) of
		(Just _, Just v) -> return (Just v)
		(Nothing, Nothing) -> return Nothing
		(Just _, Nothing) -> onemissing "annex.version" "annex.uuid"
		(Nothing, Just _) -> onemissing "annex.uuid" "annex.version"
  where
	onemissing missing have = giveup $ unwords
		[ "This repository has " ++ have ++ " set,"
		, "but " ++ missing ++ " is not set. Perhaps that"
		, "git config was lost. Cannot use the repository"
		, "in this state; set back " ++ missing ++ " to fix this."
		]

{- Will automatically initialize if there is already a git-annex
 - branch from somewhere. Otherwise, require a manual init
 - to avoid git-annex accidentally being run in git
 - repos that did not intend to use it.
 -
 - Checks repository version and handles upgrades too.
 -}
ensureInitialized :: Annex () -> Annex [Remote] -> Annex ()
ensureInitialized startupannex remotelist = getInitializedVersion >>= maybe needsinit checkUpgrade
  where
	needsinit = ifM autoInitializeAllowed
		( do
			tryNonAsync (initialize startupannex Nothing Nothing) >>= \case
				Right () -> noop
				Left e -> giveup $ show e ++ "\n" ++
					"git-annex: automatic initialization failed due to above problems"
			autoEnableSpecialRemotes remotelist
		, giveup "First run: git-annex init"
		)

{- Check if auto-initialize is allowed. -}
autoInitializeAllowed :: Annex Bool
autoInitializeAllowed = Annex.Branch.hasSibling <&&> objectDirNotPresent

objectDirNotPresent :: Annex Bool
objectDirNotPresent = do
	d <- fromRepo gitAnnexObjectDir
	exists <- liftIO $ doesDirectoryExist d
	when exists $ guardSafeToUseRepo $
		giveup $ unwords $ 
			[ "This repository is not initialized for use"
			, "by git-annex, but " ++ fromOsPath d ++ " exists,"
			, "which indicates this repository was used by"
			, "git-annex before, and may have lost its"
			, "annex.uuid and annex.version configs. Either"
			, "set back missing configs, or run git-annex init"
			, "to initialize with a new uuid."
			]
	return (not exists)

guardSafeToUseRepo :: Annex a -> Annex a
guardSafeToUseRepo a = ifM (inRepo Git.Config.checkRepoConfigInaccessible)
	( do
		repopath <- fromRepo Git.repoPath
		p <- liftIO $ absPath repopath
		giveup $ unlines $
			[ "Git refuses to operate in this repository,"
			, "probably because it is owned by someone else."
			, ""
			-- This mirrors git's wording.
			, "To add an exception for this directory, call:"
			, "\tgit config --global --add safe.directory " ++ fromOsPath p
			]
	, a
	)

{- Initialize if it can do so automatically. Avoids failing if it cannot.
 -
 - Checks repository version and handles upgrades too.
 -}
autoInitialize :: Annex () -> Annex [Remote] -> Annex ()
autoInitialize = autoInitialize' autoInitializeAllowed

autoInitialize' :: Annex Bool -> Annex () -> Annex [Remote] -> Annex ()
autoInitialize' check startupannex remotelist =
	getInitializedVersion >>= maybe needsinit checkUpgrade
  where
	needsinit =
		whenM (check <&&> initializeAllowed) $ do
			initialize startupannex Nothing Nothing
			autoEnableSpecialRemotes remotelist

{- Checks if a repository is initialized. Does not check version for upgrade. -}
isInitialized :: Annex Bool
isInitialized = maybe Annex.Branch.hasSibling (const $ return True) =<< getVersion

{- A crippled filesystem is one that does not allow making symlinks,
 - or removing write access from files.
 -
 - This displays messages about problems detected with the filesystem.
 -
 - If a freeze or thaw hook is configured, but exits nonzero,
 - this returns Nothing after displaying a message to the user about the
 - problem. Such a hook can in some cases make a filesystem
 - that would otherwise be detected as crippled work ok, so this avoids
 - a false positive.
 -}
probeCrippledFileSystem :: Annex (Maybe Bool)
probeCrippledFileSystem = do
	(r, warnings) <- isCrippledFileSystem'
	mapM_ (warning . UnquotedString) warnings
	return r

isCrippledFileSystem :: Annex Bool
isCrippledFileSystem = do
	(r, _warnings) <- isCrippledFileSystem'
	return (fromMaybe True r)

isCrippledFileSystem' :: Annex (Maybe Bool, [String])
isCrippledFileSystem' = withEventuallyCleanedOtherTmp $ \tmp ->
	probeCrippledFileSystem' tmp
		(Just (freezeContent' UnShared))
		(Just (thawContent' UnShared))
		=<< hasFreezeHook

probeCrippledFileSystem'
	:: (MonadIO m, MonadCatch m)
	=> OsPath
	-> Maybe (OsPath -> m HookResult)
	-> Maybe (OsPath -> m HookResult)
	-> Bool
	-> m (Maybe Bool, [String])
#ifdef mingw32_HOST_OS
probeCrippledFileSystem' _ _ _ _ = return (Just True, [])
#else
probeCrippledFileSystem' tmp freezecontent thawcontent hasfreezehook = do
	let f = tmp </> literalOsPath "gaprobe"
	liftIO $ F.writeFile' f ""
	r <- freezethaw f probe
	liftIO $ removeFile f
	return r
  where
	fallbackfreezecontent f = do
		liftIO $ preventWrite f
		return HookSuccess
	
	fallbackthawcontent f = do
		liftIO $ allowWrite f
		return HookSuccess
	
	freezethaw f cont =
		(fromMaybe fallbackfreezecontent freezecontent) f >>= \case
			HookFailed failedcommanddesc ->
				return (Nothing, [hookfailed failedcommanddesc])
			HookSuccess -> do
				r <- cont f
				tryNonAsync ((fromMaybe fallbackthawcontent thawcontent) f)
					>>= return . \case
						Right (HookFailed failedcommanddesc) ->
							let (_, warnings) = r
							in (Nothing, hookfailed failedcommanddesc : warnings)
						_ -> r

	hookfailed failedcommanddesc = "Failed to run " ++ failedcommanddesc 
		++ ". Unable to initialize until this is fixed."

	probe f = catchDefaultIO (Just True, []) $ do
		let f2 = f <> literalOsPath "2"
		liftIO $ removeWhenExistsWith removeFile f2
		liftIO $ R.createSymbolicLink (fromOsPath f) (fromOsPath f2)
		liftIO $ removeWhenExistsWith removeFile f2
		-- Should be unable to write to the file (unless
		-- running as root). But some crippled
		-- filesystems ignore write bit removals or ignore
		-- permissions entirely.
		ifM ((== Just False) <$> liftIO (checkContentWritePerm' UnShared f Nothing hasfreezehook))
			( return (Just True, ["Filesystem does not allow removing write bit from files."])
			, liftIO $ ifM ((== 0) <$> getRealUserID)
				( return (Just False, [])
				, do
					r <- catchBoolIO $ do
						F.writeFile' f "2"
						return True
					if r
						then return (Just True, ["Filesystem allows writing to files whose write bit is not set."])
						else return (Just False, [])
				)
			)
#endif

checkCrippledFileSystem :: Annex ()
checkCrippledFileSystem = probeCrippledFileSystem >>= \case
	Just True -> do
		warning "Detected a crippled filesystem."
		setCrippledFileSystem True

		{- Normally git disables core.symlinks itself when the
		 -
		 - filesystem does not support them. But, even if symlinks are
		 - supported, we don't use them by default in a crippled
		 - filesystem. -}
		whenM (coreSymlinks <$> Annex.getGitConfig) $ do
			warning "Disabling core.symlinks."
			setConfig "core.symlinks"
				(Git.Config.boolConfig False)
	Just False -> noop
	Nothing -> giveup "Not initialized."

probeLockSupport :: Annex Bool
#ifdef mingw32_HOST_OS
probeLockSupport = return True
#else
probeLockSupport = withEventuallyCleanedOtherTmp $ \tmp -> do
	let f = tmp </> literalOsPath "lockprobe"
	mode <- annexFileMode
	annexrunner <- Annex.makeRunner
	liftIO $ withAsync (warnstall annexrunner) (const (go f mode))
  where
	go f mode = do
		removeWhenExistsWith removeFile f
		let locktest = bracket
			(Posix.lockExclusive (Just mode) f)
			Posix.dropLock
			(const noop)
		ok <- isRight <$> tryNonAsync locktest
		removeWhenExistsWith removeFile f
		return ok
	
	warnstall annexrunner = do
		threadDelaySeconds (Seconds 10)
		annexrunner $ do
			warning "Probing the filesystem for POSIX fcntl lock support is taking a long time."
			warning "(Setting annex.pidlock will avoid this probe.)"
#endif

probeFifoSupport :: Annex Bool
probeFifoSupport = do
#ifdef mingw32_HOST_OS
	return False
#else
	withEventuallyCleanedOtherTmp $ \tmp -> do
		let f = tmp </> literalOsPath "gaprobe"
		let f2 = tmp </> literalOsPath "gaprobe2"
		liftIO $ do
			removeWhenExistsWith removeFile f
			removeWhenExistsWith removeFile f2
			ms <- tryIO $ do
				R.createNamedPipe (fromOsPath f) ownerReadMode
				R.createLink (fromOsPath f) (fromOsPath f2)
				R.getFileStatus (fromOsPath f)
			removeWhenExistsWith removeFile f
			removeWhenExistsWith removeFile f2
			return $ either (const False) isNamedPipe ms
#endif

checkLockSupport :: Annex ()
checkLockSupport =
	unlessM (annexPidLock <$> Annex.getGitConfig) $
		unlessM probeLockSupport $ do
			warning "Detected a filesystem without POSIX fcntl lock support."
			warning "Enabling annex.pidlock."
			setConfig (annexConfig "pidlock") (Git.Config.boolConfig True)

checkFifoSupport :: Annex ()
checkFifoSupport = unlessM probeFifoSupport $ do
	warning "Detected a filesystem without fifo support."
	warning "Disabling ssh connection caching."
	setConfig (annexConfig "sshcaching") (Git.Config.boolConfig False)

{- Sqlite needs the filesystem to support range locking. Some like CIFS
 - do not, which will cause sqlite to fail with ErrorBusy. -}
checkSqliteWorks :: Annex ()
checkSqliteWorks = do
	u <- getUUID
	tryNonAsync (Database.Fsck.openDb u >>= Database.Fsck.closeDb) >>= \case
		Right () -> return ()
		Left e -> do
			showLongNote $ "Detected a filesystem where Sqlite does not work."
			showLongNote $ UnquotedString $ "(" ++ show e ++ ")"
			showLongNote $ "To work around this problem, you can set annex.dbdir " <>
				"to a directory on another filesystem."
			showLongNote $ "For example: git config annex.dbdir $HOME/cache/git-annex"
			giveup "Not initialized."

checkSharedClone :: Annex Bool
checkSharedClone = inRepo Git.Objects.isSharedClone

initSharedClone :: Bool -> Annex ()
initSharedClone False = return ()
initSharedClone True = do
	showLongNote "Repository was cloned with --shared; setting annex.hardlink=true and making repository untrusted."
	u <- getUUID
	trustSet u UnTrusted
	setConfig (annexConfig "hardlink") (Git.Config.boolConfig True)

{- Propigate annex.securehashesonly from the global config to local
 - config. This makes a clone inherit a parent's setting, but once
 - a repository has a local setting, changes to the global config won't
 - affect it. -}
propigateSecureHashesOnly :: Annex ()
propigateSecureHashesOnly =
	maybe noop (setConfig "annex.securehashesonly" . fromConfigValue)
		=<< getGlobalConfig "annex.securehashesonly"

{- Propigate git configs that set defaults. -}
propigateInitGitConfigs :: UUID -> Annex ()
propigateInitGitConfigs u = do
	gc <- Annex.getGitConfig
	set (annexInitWanted gc) preferredContentSet
	set (annexInitRequired gc) requiredContentSet
	case annexInitGroups gc of
		[] -> noop
		groups -> groupChange u (S.union (S.fromList groups))
  where
	set (Just expr) setter = setter u expr
	set Nothing _ = noop

fixupUnusualReposAfterInit :: Annex ()
fixupUnusualReposAfterInit = do
	gc <- Annex.getGitConfig
	void $ inRepo $ \r -> fixupUnusualRepos r gc

{- Try to enable any special remotes that are configured to do so.
 - 
 - The enabling is done in a child process to avoid it using stdio.
 -
 - The remotelist should be Remote.List.remoteList, which cannot
 - be imported here due to a dependency loop.
 -}
autoEnableSpecialRemotes :: Annex [Remote] -> Annex ()
autoEnableSpecialRemotes remotelist = do
	-- Get all existing git remotes to probe for their uuid here,
	-- so it is not done inside the child process. Doing it in there
	-- could result in password prompts for http credentials,
	-- which would then not end up cached in this process's state.
	_ <- remotelist
	rp <- fromRepo Git.repoPath
	withNullHandle $ \nullh -> gitAnnexChildProcess "init"
		[ Param "--autoenable" ]
		(\p -> p
			{ std_out = UseHandle nullh
			, std_err = UseHandle nullh
			, std_in = UseHandle nullh
			, cwd = Just (fromOsPath rp)
			}
		)
		(\_ _ _ pid -> void $ waitForProcess pid)
	remotesChanged
