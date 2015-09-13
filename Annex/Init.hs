{- git-annex repository initialization
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Init (
	ensureInitialized,
	isInitialized,
	initialize,
	initialize',
	uninitialize,
	probeCrippledFileSystem,
) where

import Common.Annex
import qualified Annex
import qualified Git
import qualified Git.LsFiles
import qualified Git.Config
import qualified Git.Objects
import qualified Annex.Branch
import Logs.UUID
import Logs.Trust.Basic
import Types.TrustLevel
import Annex.Version
import Annex.Difference
import Annex.UUID
import Config
import Annex.Direct
import Annex.Content.Direct
import Annex.Environment
import Backend
import Annex.Hook
import Upgrade
#ifndef mingw32_HOST_OS
import Utility.UserInfo
import Utility.FileMode
import Annex.Perms
import System.Posix.User
#endif

genDescription :: Maybe String -> Annex String
genDescription (Just d) = return d
genDescription Nothing = do
	reldir <- liftIO . relHome =<< liftIO . absPath =<< fromRepo Git.repoPath
	hostname <- fromMaybe "" <$> liftIO getHostname
#ifndef mingw32_HOST_OS
	let at = if null hostname then "" else "@"
	username <- liftIO myUserName
	return $ concat [username, at, hostname, ":", reldir]
#else
	return $ concat [hostname, ":", reldir]
#endif

initialize :: Maybe String -> Annex ()
initialize mdescription = do
	{- This will make the first commit to git, so ensure git is set up
	 - properly to allow commits when running it. -}
	ensureCommit $ Annex.Branch.create

	prepUUID
	initialize'

	u <- getUUID
	describeUUID u =<< genDescription mdescription

-- Everything except for uuid setup.
initialize' :: Annex ()
initialize' = do
	checkFifoSupport
	checkCrippledFileSystem
	unlessM isBare $
		hookWrite preCommitHook
	setDifferences
	setVersion supportedVersion
	ifM (crippledFileSystem <&&> not <$> isBare)
		( do
			enableDirectMode
			setDirect True
		-- Handle case where this repo was cloned from a
		-- direct mode repo
		, unlessM isBare
			switchHEADBack
		)
	createInodeSentinalFile
	checkSharedClone

uninitialize :: Annex ()
uninitialize = do
	hookUnWrite preCommitHook
	removeRepoUUID
	removeVersion

{- Will automatically initialize if there is already a git-annex
 - branch from somewhere. Otherwise, require a manual init
 - to avoid git-annex accidentially being run in git
 - repos that did not intend to use it.
 -
 - Checks repository version and handles upgrades too.
 -}
ensureInitialized :: Annex ()
ensureInitialized = getVersion >>= maybe needsinit checkUpgrade
  where
	needsinit = ifM Annex.Branch.hasSibling
			( initialize Nothing
			, error "First run: git-annex init"
			)

{- Checks if a repository is initialized. Does not check version for ugrade. -}
isInitialized :: Annex Bool
isInitialized = maybe Annex.Branch.hasSibling (const $ return True) =<< getVersion

isBare :: Annex Bool
isBare = fromRepo Git.repoIsLocalBare

{- A crippled filesystem is one that does not allow making symlinks,
 - or removing write access from files. -}
probeCrippledFileSystem :: Annex Bool
probeCrippledFileSystem = do
#ifdef mingw32_HOST_OS
	return True
#else
	tmp <- fromRepo gitAnnexTmpMiscDir
	let f = tmp </> "gaprobe"
	createAnnexDirectory tmp
	liftIO $ writeFile f ""
	uncrippled <- liftIO $ probe f
	void $ liftIO $ tryIO $ allowWrite f
	liftIO $ removeFile f
	return $ not uncrippled
  where
	probe f = catchBoolIO $ do
		let f2 = f ++ "2"
		nukeFile f2
		createSymbolicLink f f2
		nukeFile f2
		preventWrite f
		-- Should be unable to write to the file, unless
		-- running as root, but some crippled
		-- filesystems ignore write bit removals.
		ifM ((== 0) <$> getRealUserID)
			( return True
			, not <$> catchBoolIO (writeFile f "2" >> return True)
			)
#endif

checkCrippledFileSystem :: Annex ()
checkCrippledFileSystem = whenM probeCrippledFileSystem $ do
	warning "Detected a crippled filesystem."
	setCrippledFileSystem True

	{- Normally git disables core.symlinks itself when the
	 - filesystem does not support them, but in Cygwin, git
	 - does support symlinks, while git-annex, not linking
	 - with Cygwin, does not. -}
	whenM (coreSymlinks <$> Annex.getGitConfig) $ do
		warning "Disabling core.symlinks."
		setConfig (ConfigKey "core.symlinks")
			(Git.Config.boolConfig False)

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

checkFifoSupport :: Annex ()
checkFifoSupport = unlessM probeFifoSupport $ do
	warning "Detected a filesystem without fifo support."
	warning "Disabling ssh connection caching."
	setConfig (annexConfig "sshcaching") (Git.Config.boolConfig False)

enableDirectMode :: Annex ()
enableDirectMode = unlessM isDirect $ do
	warning "Enabling direct mode."
	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.inRepo [top]
	forM_ l $ \f ->
		maybe noop (`toDirect` f) =<< isAnnexLink f
	void $ liftIO clean

checkSharedClone :: Annex ()
checkSharedClone = whenM (inRepo Git.Objects.isSharedClone) $ do
	showSideAction "Repository was cloned with --shared; setting annex.hardlink=true and making repository untrusted."
	u <- getUUID
	trustSet u UnTrusted
	setConfig (annexConfig "hardlink") (Git.Config.boolConfig True)
