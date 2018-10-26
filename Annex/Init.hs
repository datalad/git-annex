{- git-annex repository initialization
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Init (
	AutoInit(..),
	ensureInitialized,
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
import Config
import Config.Smudge
import Annex.Direct
import Annex.AdjustedBranch
import Annex.Environment
import Annex.Hook
import Annex.InodeSentinal
import Upgrade
import Annex.Perms
import Utility.UserInfo
#ifndef mingw32_HOST_OS
import Utility.FileMode
import System.Posix.User
import qualified Utility.LockFile.Posix as Posix
#endif

newtype AutoInit = AutoInit Bool

checkCanInitialize :: AutoInit -> Annex a -> Annex a
checkCanInitialize (AutoInit True) a = a
checkCanInitialize (AutoInit False) a = fromRepo Git.repoWorkTree >>= \case
	Nothing -> a
	Just wt -> liftIO (catchMaybeIO (readFile (wt </> ".noannex"))) >>= \case
		Nothing -> a
		Just noannexmsg -> ifM (Annex.getState Annex.force)
			( a
			, do
				warning "Initialization prevented by .noannex file (use --force to override)"
				unless (null noannexmsg) $
					warning noannexmsg
				giveup "Not initialized."
			)

genDescription :: Maybe String -> Annex String
genDescription (Just d) = return d
genDescription Nothing = do
	reldir <- liftIO . relHome =<< liftIO . absPath =<< fromRepo Git.repoPath
	hostname <- fromMaybe "" <$> liftIO getHostname
	let at = if null hostname then "" else "@"
	v <- liftIO myUserName
	return $ concat $ case v of
		Right username -> [username, at, hostname, ":", reldir]
		Left _ -> [hostname, ":", reldir]

initialize :: AutoInit -> Maybe String -> Maybe RepoVersion -> Annex ()
initialize ai mdescription mversion = checkCanInitialize ai $ do
	{- Has to come before any commits are made as the shared
	 - clone heuristic expects no local objects. -}
	sharedclone <- checkSharedClone

	{- This will make the first commit to git, so ensure git is set up
	 - properly to allow commits when running it. -}
	ensureCommit $ Annex.Branch.create

	prepUUID
	initialize' (AutoInit True) mversion
	
	initSharedClone sharedclone

	u <- getUUID
	describeUUID u =<< genDescription mdescription

-- Everything except for uuid setup, shared clone setup, and initial
-- description.
initialize' :: AutoInit -> Maybe RepoVersion -> Annex ()
initialize' ai mversion = checkCanInitialize ai $ do
	checkLockSupport
	checkFifoSupport
	checkCrippledFileSystem
	unlessM isBareRepo $ do
		hookWrite preCommitHook
		hookWrite postReceiveHook
	setDifferences
	unlessM (isJust <$> getVersion) $
		setVersion (fromMaybe defaultVersion mversion)
	whenM versionSupportsUnlockedPointers $ do
		configureSmudgeFilter
		scanUnlockedFiles
		unlessM isBareRepo $ do
			hookWrite postCheckoutHook
			hookWrite postMergeHook
	checkAdjustedClone >>= \case
		NeedUpgradeForAdjustedClone -> 
			void $ upgrade True versionForAdjustedClone
		InAdjustedClone -> return ()
		NotInAdjustedClone ->
			ifM (crippledFileSystem <&&> (not <$> isBareRepo))
				( adjustToCrippledFileSystem
				-- Handle case where this repo was cloned from a
				-- direct mode repo
				, unlessM isBareRepo
					switchHEADBack
				)
	propigateSecureHashesOnly
	createInodeSentinalFile False

uninitialize :: Annex ()
uninitialize = do
	hookUnWrite preCommitHook
	hookUnWrite postReceiveHook
	removeRepoUUID
	removeVersion

{- Will automatically initialize if there is already a git-annex
 - branch from somewhere. Otherwise, require a manual init
 - to avoid git-annex accidentally being run in git
 - repos that did not intend to use it.
 -
 - Checks repository version and handles upgrades too.
 -}
ensureInitialized :: Annex ()
ensureInitialized = getVersion >>= maybe needsinit checkUpgrade
  where
	needsinit = ifM Annex.Branch.hasSibling
			( initialize (AutoInit True) Nothing Nothing
			, giveup "First run: git-annex init"
			)

{- Checks if a repository is initialized. Does not check version for ugrade. -}
isInitialized :: Annex Bool
isInitialized = maybe Annex.Branch.hasSibling (const $ return True) =<< getVersion

{- A crippled filesystem is one that does not allow making symlinks,
 - or removing write access from files. -}
probeCrippledFileSystem :: Annex Bool
probeCrippledFileSystem = do
	tmp <- fromRepo gitAnnexTmpMiscDir
	createAnnexDirectory tmp
	(r, warnings) <- liftIO $ probeCrippledFileSystem' tmp
	mapM_ warning warnings
	return r

probeCrippledFileSystem' :: FilePath -> IO (Bool, [String])
#ifdef mingw32_HOST_OS
probeCrippledFileSystem' _ = return (True, [])
#else
probeCrippledFileSystem' tmp = do
	let f = tmp </> "gaprobe"
	writeFile f ""
	r <- probe f
	void $ tryIO $ allowWrite f
	removeFile f
	return r
  where
	probe f = catchDefaultIO (True, []) $ do
		let f2 = f ++ "2"
		nukeFile f2
		createSymbolicLink f f2
		nukeFile f2
		preventWrite f
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
		setConfig (ConfigKey "core.symlinks")
			(Git.Config.boolConfig False)

probeLockSupport :: Annex Bool
probeLockSupport = do
#ifdef mingw32_HOST_OS
	return True
#else
	tmp <- fromRepo gitAnnexTmpMiscDir
	let f = tmp </> "lockprobe"
	createAnnexDirectory tmp
	mode <- annexFileMode
	liftIO $ do
		nukeFile f
		ok <- catchBoolIO $ do
			Posix.dropLock =<< Posix.lockExclusive (Just mode) f
			return True
		nukeFile f
		return ok
#endif

probeFifoSupport :: Annex Bool
probeFifoSupport = do
#ifdef mingw32_HOST_OS
	return False
#else
	tmp <- fromRepo gitAnnexTmpMiscDir
	let f = tmp </> "gaprobe"
	let f2 = tmp </> "gaprobe2"
	createAnnexDirectory tmp
	liftIO $ do
		nukeFile f
		nukeFile f2
		ms <- tryIO $ do
			createNamedPipe f ownerReadMode
			createLink f f2
			getFileStatus f
		nukeFile f
		nukeFile f2
		return $ either (const False) isNamedPipe ms
#endif

checkLockSupport :: Annex ()
checkLockSupport = unlessM probeLockSupport $ do
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
	maybe noop (setConfig (ConfigKey "annex.securehashesonly"))
		=<< getGlobalConfig "annex.securehashesonly"
