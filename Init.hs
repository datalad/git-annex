{- git-annex repository initialization
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Init (
	ensureInitialized,
	isInitialized,
	initialize,
	uninitialize,
	probeCrippledFileSystem,
) where

import Common.Annex
import Utility.Network
import qualified Annex
import qualified Git
import qualified Git.LsFiles
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Types as Git
import qualified Annex.Branch
import Logs.UUID
import Annex.Version
import Annex.UUID
import Config
import Annex.Direct
import Annex.Content.Direct
import Annex.Environment
import Annex.Perms
import Backend
#ifndef mingw32_HOST_OS
import Utility.UserInfo
import Utility.FileMode
#endif
import Annex.Hook
import Git.Hook (hookFile)
import Upgrade
import Annex.Content
import Logs.Location

import System.Log.Logger

genDescription :: Maybe String -> Annex String
genDescription (Just d) = return d
genDescription Nothing = do
	reldir <- liftIO . relHome =<< fromRepo Git.repoPath
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
	prepUUID
	checkFifoSupport
	checkCrippledFileSystem
	unlessM isBare $
		hookWrite preCommitHook
	ifM (crippledFileSystem <&&> not <$> isBare)
		( do
			enableDirectMode
			setDirect True
			setVersion directModeVersion
		, do
			setVersion defaultVersion
			-- Handle case where this repo was cloned from a
			-- direct mode repo.
			unlessM isBare
				switchHEADBack
		)
	createInodeSentinalFile
	u <- getUUID
	{- This will make the first commit to git, so ensure git is set up
	 - properly to allow commits when running it. -}
	ensureCommit $ do
		Annex.Branch.create
		describeUUID u =<< genDescription mdescription

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
ensureInitialized = do
	getVersion >>= maybe needsinit checkUpgrade
	fixBadBare
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
	tmp <- fromRepo gitAnnexTmpDir
	let f = tmp </> "gaprobe"
	createAnnexDirectory tmp
	liftIO $ writeFile f ""
	uncrippled <- liftIO $ probe f
	liftIO $ removeFile f
	return $ not uncrippled
  where
	probe f = catchBoolIO $ do
		let f2 = f ++ "2"
		nukeFile f2
		createSymbolicLink f f2
		nukeFile f2
		preventWrite f
		allowWrite f
		return True
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
	tmp <- fromRepo gitAnnexTmpDir
	let f = tmp </> "gaprobe"
	createAnnexDirectory tmp
	liftIO $ do
		nukeFile f
		ms <- tryIO $ do
			createNamedPipe f ownerReadMode
			getFileStatus f
		nukeFile f
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

{- Work around for git-annex version 5.20131118 - 5.20131127, which
 - had a bug that unset core.bare when initializing a bare repository.
 - 
 - This resulted in objects sent to the repository being stored in 
 - repo/.git/annex/objects, so move them to repo/annex/objects.
 -
 - This check slows down every git-annex run somewhat (by one file stat),
 - so should be removed after a suitable period of time has passed.
 - Since the bare repository may be on an offline USB drive, best to
 - keep it for a while. However, git-annex was only buggy for a few
 - weeks, so not too long.
 -}
fixBadBare :: Annex ()
fixBadBare = whenM checkBadBare $ do
	ks <- getKeysPresent
	liftIO $ debugM "Init" $ unwords
		[ "Detected bad bare repository with"
		, show (length ks)
		, "objects; fixing"
		]
	g <- Annex.gitRepo
	gc <- Annex.getGitConfig
	d <- Git.repoPath <$> Annex.gitRepo
	void $ liftIO $ boolSystem "git"
		[ Param $ "--git-dir=" ++ d
		, Param "config"
		, Param Git.Config.coreBare
		, Param $ Git.Config.boolConfig True
		]
	g' <- liftIO $ Git.Construct.fromPath d
	s' <- liftIO $ Annex.new $ g' { Git.location = Git.Local { Git.gitdir = d, Git.worktree = Nothing } }
	Annex.changeState $ \s -> s
		{ Annex.repo = Annex.repo s'
		, Annex.gitconfig = Annex.gitconfig s'
		}
	forM_ ks $ \k -> do
		oldloc <- liftIO $ gitAnnexLocation k g gc
		thawContentDir oldloc
		moveAnnex k oldloc
		logStatus k InfoPresent
	let dotgit = d </> ".git"
	liftIO $ removeDirectoryRecursive dotgit
		`catchIO` (const $ renameDirectory dotgit (d </> "removeme"))

{- A repostory with the problem won't know it's a bare repository, but will
 - have no pre-commit hook (which is not set up in a bare repository),
 - and will not have a HEAD file in its .git directory. -}
checkBadBare :: Annex Bool
checkBadBare = allM (not <$>)
	[isBare, hasPreCommitHook, hasDotGitHEAD]
  where
	hasPreCommitHook = inRepo $ doesFileExist . hookFile preCommitHook
	hasDotGitHEAD = inRepo $ \r -> doesFileExist $ Git.localGitDir r </> "HEAD"
