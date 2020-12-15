{- git-annex repository initialization
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Annex.Init (
	ensureInitialized,
	autoInitialize,
	isInitialized,
	initialize,
	initialize',
	uninitialize,
	probeCrippledFileSystem,
	probeCrippledFileSystem',
) where

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.Config
import qualified Git.Objects
import Git.Types (fromConfigValue)
import qualified Annex.Branch
import Logs.UUID
import Logs.Trust.Basic
import Logs.Config
import Types.TrustLevel
import Types.RepoVersion
import Annex.Version
import Annex.Difference
import Annex.UUID
import Annex.WorkTree
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
import qualified Utility.RawFilePath as R
#ifndef mingw32_HOST_OS
import Utility.ThreadScheduler
import Annex.Perms
import Utility.FileMode
import System.Posix.User
import qualified Utility.LockFile.Posix as Posix
#endif

import qualified Data.Map as M
#ifndef mingw32_HOST_OS
import Data.Either
import qualified System.FilePath.ByteString as P
import Control.Concurrent.Async
#endif

checkInitializeAllowed :: Annex a -> Annex a
checkInitializeAllowed a = noAnnexFileContent' >>= \case
	Nothing -> a
	Just noannexmsg -> do
		warning "Initialization prevented by .noannex file (remove the file to override)"
		unless (null noannexmsg) $
			warning noannexmsg
		giveup "Not initialized."

initializeAllowed :: Annex Bool
initializeAllowed = isNothing <$> noAnnexFileContent'

noAnnexFileContent' :: Annex (Maybe String)
noAnnexFileContent' = inRepo $
	noAnnexFileContent . fmap fromRawFilePath . Git.repoWorkTree

genDescription :: Maybe String -> Annex UUIDDesc
genDescription (Just d) = return $ UUIDDesc $ encodeBS d
genDescription Nothing = do
	reldir <- liftIO . relHome . fromRawFilePath
		=<< liftIO . absPath
		=<< fromRepo Git.repoPath
	hostname <- fromMaybe "" <$> liftIO getHostname
	let at = if null hostname then "" else "@"
	v <- liftIO myUserName
	return $ UUIDDesc $ encodeBS $ concat $ case v of
		Right username -> [username, at, hostname, ":", reldir]
		Left _ -> [hostname, ":", reldir]

initialize :: Maybe String -> Maybe RepoVersion -> Annex ()
initialize mdescription mversion = checkInitializeAllowed $ do
	{- Has to come before any commits are made as the shared
	 - clone heuristic expects no local objects. -}
	sharedclone <- checkSharedClone

	{- This will make the first commit to git, so ensure git is set up
	 - properly to allow commits when running it. -}
	ensureCommit $ Annex.Branch.create

	prepUUID
	initialize' mversion
	
	initSharedClone sharedclone

	u <- getUUID
	{- Avoid overwriting existing description with a default
	 - description. -}
	whenM (pure (isJust mdescription) <||> not . M.member u <$> uuidDescMapRaw) $
		describeUUID u =<< genDescription mdescription

-- Everything except for uuid setup, shared clone setup, and initial
-- description.
initialize' :: Maybe RepoVersion -> Annex ()
initialize' mversion = checkInitializeAllowed $ do
	checkLockSupport
	checkFifoSupport
	checkCrippledFileSystem
	unlessM isBareRepo $ do
		hookWrite preCommitHook
		hookWrite postReceiveHook
	setDifferences
	unlessM (isJust <$> getVersion) $
		setVersion (fromMaybe defaultVersion mversion)
	configureSmudgeFilter
	unlessM isBareRepo $ do
		showSideAction "scanning for unlocked files"
		scanUnlockedFiles
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
	createInodeSentinalFile False
	fixupUnusualReposAfterInit

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
ensureInitialized :: Annex ()
ensureInitialized = getInitializedVersion >>= maybe needsinit checkUpgrade
  where
	needsinit = ifM autoInitializeAllowed
		( do
			initialize Nothing Nothing
			autoEnableSpecialRemotes
		, giveup "First run: git-annex init"
		)

{- Check if auto-initialize is allowed. -}
autoInitializeAllowed :: Annex Bool
autoInitializeAllowed = Annex.Branch.hasSibling <&&> objectDirNotPresent

objectDirNotPresent :: Annex Bool
objectDirNotPresent = do
	d <- fromRawFilePath <$> fromRepo gitAnnexObjectDir
	exists <- liftIO $ doesDirectoryExist d
	when exists $
		giveup $ unwords $ 
			[ "This repository is not initialized for use"
			, "by git-annex, but " ++ d ++ " exists,"
			, "which indicates this repository was used by"
			, "git-annex before, and may have lost its"
			, "annex.uuid and annex.version configs. Either"
			, "set back missing configs, or run git-annex init"
			, "to initialize with a new uuid."
			]
	return (not exists)

{- Initialize if it can do so automatically. Avoids failing if it cannot.
 -
 - Checks repository version and handles upgrades too.
 -}
autoInitialize :: Annex ()
autoInitialize = getInitializedVersion >>= maybe needsinit checkUpgrade
  where
	needsinit =
		whenM (initializeAllowed <&&> autoInitializeAllowed) $ do
			initialize Nothing Nothing
			autoEnableSpecialRemotes

{- Checks if a repository is initialized. Does not check version for ugrade. -}
isInitialized :: Annex Bool
isInitialized = maybe Annex.Branch.hasSibling (const $ return True) =<< getVersion

{- A crippled filesystem is one that does not allow making symlinks,
 - or removing write access from files. -}
probeCrippledFileSystem :: Annex Bool
probeCrippledFileSystem = withEventuallyCleanedOtherTmp $ \tmp -> do
	(r, warnings) <- liftIO $ probeCrippledFileSystem' tmp
	mapM_ warning warnings
	return r

probeCrippledFileSystem' :: RawFilePath -> IO (Bool, [String])
#ifdef mingw32_HOST_OS
probeCrippledFileSystem' _ = return (True, [])
#else
probeCrippledFileSystem' tmp = do
	let f = fromRawFilePath (tmp P.</> "gaprobe")
	writeFile f ""
	r <- probe f
	void $ tryIO $ allowWrite (toRawFilePath f)
	removeFile f
	return r
  where
	probe f = catchDefaultIO (True, []) $ do
		let f2 = f ++ "2"
		removeWhenExistsWith R.removeLink (toRawFilePath f2)
		createSymbolicLink f f2
		removeWhenExistsWith R.removeLink (toRawFilePath f2)
		preventWrite (toRawFilePath f)
		-- Should be unable to write to the file, unless
		-- running as root, but some crippled
		-- filesystems ignore write bit removals.
		ifM ((== 0) <$> getRealUserID)
			( return (False, [])
			, do
				r <- catchBoolIO $ do
					writeFile f "2"
					return True
				if r
					then return (True, ["Filesystem allows writing to files whose write bit is not set."])
					else return (False, [])
			)
#endif

checkCrippledFileSystem :: Annex ()
checkCrippledFileSystem = whenM probeCrippledFileSystem $ do
	warning "Detected a crippled filesystem."
	setCrippledFileSystem True

	{- Normally git disables core.symlinks itself when the
	 - filesystem does not support them. But, even if symlinks are
	 - supported, we don't use them by default in a crippled
	 - filesystem. -}
	whenM (coreSymlinks <$> Annex.getGitConfig) $ do
		warning "Disabling core.symlinks."
		setConfig "core.symlinks"
			(Git.Config.boolConfig False)

probeLockSupport :: Annex Bool
#ifdef mingw32_HOST_OS
probeLockSupport = return True
#else
probeLockSupport = withEventuallyCleanedOtherTmp $ \tmp -> do
	let f = tmp P.</> "lockprobe"
	mode <- annexFileMode
	annexrunner <- Annex.makeRunner
	liftIO $ withAsync (warnstall annexrunner) (const (go f mode))
  where
	go f mode = do
		removeWhenExistsWith R.removeLink f
		let locktest = bracket
			(Posix.lockExclusive (Just mode) f)
			Posix.dropLock
			(const noop)
		ok <- isRight <$> tryNonAsync locktest
		removeWhenExistsWith R.removeLink f
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
		let f = tmp P.</> "gaprobe"
		let f2 = tmp P.</> "gaprobe2"
		liftIO $ do
			removeWhenExistsWith R.removeLink f
			removeWhenExistsWith R.removeLink f2
			ms <- tryIO $ do
				createNamedPipe (fromRawFilePath f) ownerReadMode
				R.createLink f f2
				R.getFileStatus f
			removeWhenExistsWith R.removeLink f
			removeWhenExistsWith R.removeLink f2
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

checkSharedClone :: Annex Bool
checkSharedClone = inRepo Git.Objects.isSharedClone

initSharedClone :: Bool -> Annex ()
initSharedClone False = return ()
initSharedClone True = do
	showLongNote "Repository was cloned with --shared; setting annex.hardlink=true and making repository untrusted."
	u <- getUUID
	trustSet u UnTrusted
	setConfig (annexConfig "hardlink") (Git.Config.boolConfig True)

{- Propigate annex.securehashesonly from then global config to local
 - config. This makes a clone inherit a parent's setting, but once
 - a repository has a local setting, changes to the global config won't
 - affect it. -}
propigateSecureHashesOnly :: Annex ()
propigateSecureHashesOnly =
	maybe noop (setConfig "annex.securehashesonly" . fromConfigValue)
		=<< getGlobalConfig "annex.securehashesonly"

fixupUnusualReposAfterInit :: Annex ()
fixupUnusualReposAfterInit = do
	gc <- Annex.getGitConfig
	void $ inRepo $ \r -> fixupUnusualRepos r gc

{- Try to enable any special remotes that are configured to do so.
 - 
 - The enabling is done in a child process to avoid it using stdio.
 -}
autoEnableSpecialRemotes :: Annex ()
autoEnableSpecialRemotes = do
	rp <- fromRawFilePath <$> fromRepo Git.repoPath
	withNullHandle $ \nullh -> gitAnnexChildProcess "init"
		[ Param "--autoenable" ]
		(\p -> p
			{ std_out = UseHandle nullh
			, std_err = UseHandle nullh
			, std_in = UseHandle nullh
			, cwd = Just rp
			}
		)
		(\_ _ _ pid -> void $ waitForProcess pid)
	remotesChanged
