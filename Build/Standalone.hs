{- Makes standalone bundle.
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Control.Monad.IfElse
import System.FilePath
import System.Posix.Files
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Utility.SafeCommand
import Utility.Process
import Utility.Path
import Utility.Directory
import Utility.Env
import Build.BundledPrograms
#ifdef darwin_HOST_OS
import Build.OSXMkLibs (mklibs)
import Build.Version
import Utility.Split
#else
import Build.LinuxMkLibs (mklibs)
import Utility.FileMode
#endif

progDir :: FilePath -> FilePath
#ifdef darwin_HOST_OS
progDir topdir = topdir
#else
progDir topdir = topdir </> "bin"
#endif

extraProgDir :: FilePath -> FilePath
extraProgDir topdir = topdir </> "extra"

installProg :: FilePath -> FilePath -> IO (FilePath, FilePath)
installProg dir prog = searchPath prog >>= go
  where
	go Nothing = error $ "cannot find " ++ prog ++ " in PATH"
	go (Just f) = do
		let dest = dir </> takeFileName f
		unlessM (boolSystem "install" [File f, File dest]) $
			error $ "install failed for " ++ prog
		return (dest, f)

installBundledPrograms :: FilePath -> IO (M.Map FilePath FilePath)
installBundledPrograms topdir = M.fromList . concat <$> mapM go
	[ (progDir topdir, preferredBundledPrograms)
	, (extraProgDir topdir, extraBundledPrograms)
	]
  where
	go (dir, progs) = do
		createDirectoryIfMissing True dir
		forM progs $ installProg dir

installGitLibs :: FilePath -> IO ()
installGitLibs topdir = do
	-- install git-core programs; these are run by the git command
	createDirectoryIfMissing True gitcoredestdir
	execpath <- getgitpath "exec-path"
	cfs <- dirContents execpath
	forM_ cfs $ \f -> do
		destf <- (gitcoredestdir </>)
			<$> relPathDirToFile execpath f
		createDirectoryIfMissing True (takeDirectory destf)
		issymlink <- isSymbolicLink <$> getSymbolicLinkStatus f
		if issymlink
			then do
				-- many git-core files may symlink to eg
				-- ../../bin/git, which is located outside
				-- the git-core directory. The target of
				-- such links is installed into the progDir
				-- (if not already there), and the links
				-- repointed to it.
				--
				-- Other git-core files symlink to a file
				-- beside them in the directory. Those
				-- links can be copied as-is.
				linktarget <- readSymbolicLink f
				if takeFileName linktarget == linktarget
					then cp f destf
					else do
						let linktarget' = progDir topdir </> takeFileName linktarget
						unlessM (doesFileExist linktarget') $ do
							createDirectoryIfMissing True (takeDirectory linktarget')
							L.readFile f >>= L.writeFile linktarget'
						nukeFile destf
						rellinktarget <- relPathDirToFile (takeDirectory destf) linktarget'
						createSymbolicLink rellinktarget destf
			else cp f destf
	
	-- install git's template files
	-- git does not have an option to get the path of these,
	-- but they're architecture independent files, so are located
	-- next to the --man-path, in eg /usr/share/git-core
	manpath <- getgitpath "man-path"
	let templatepath = manpath </> ".." </> "git-core" </> "templates"
	tfs <- dirContents templatepath
	forM_ tfs $ \f -> do
		destf <- (templatedestdir </>)
			<$> relPathDirToFile templatepath f
		createDirectoryIfMissing True (takeDirectory destf)
		cp f destf
  where
	gitcoredestdir = topdir </> "git-core"
	templatedestdir = topdir </> "templates"

	getgitpath v = do
		let opt = "--" ++ v
		ls <- lines <$> readProcess "git" [opt]
		case ls of
			[] -> error $ "git " ++ opt ++ "did not output a location"
			(p:_) -> return p

cp :: FilePath -> FilePath -> IO ()
cp src dest = do
	nukeFile dest
	unlessM (boolSystem "cp" [Param "-a", File src, File dest]) $
		error "cp failed"

installMagic :: FilePath -> IO ()
#ifdef darwin_HOST_OS
installMagic topdir = getEnv "OSX_MAGIC_FILE" >>= \case
	Nothing -> hputStrLn stderr "OSX_MAGIC_FILE not set; not including it"
	Just f -> do
		let mdir = topdir </> "magic"
		createDirectoryIfMissing True mdir
		unlessM (boolSystem "cp" [File f, File mdir </> "magic.mgc") $
			error "cp failed"
#else
installMagic topdir = do
	let mdir = topdir </> "magic"
	createDirectoryIfMissing True mdir
	unlessM (boolSystem "cp" [File "/usr/share/file/magic.mgc", File (mdir </> "magic.mgc")]) $
		error "cp failed"
#endif

installLocales :: FilePath -> IO ()
#ifdef darwin_HOST_OS
installLocales _ = return ()
#else
installLocales topdir = cp "/usr/share/i18n" (topdir </> "i18n")
#endif

installSkel :: FilePath -> FilePath -> IO ()
#ifdef darwin_HOST_OS
installSkel topdir basedir = do
	whenM (doesDirectoryExist basedir) $
		removeDirectoryRecursive basedir
	createDirectoryIfMissing True (takeDirectory basedir)
	unlessM (boolSystem "cp" [Param "-R", File "standalone/osx/git-annex.app", File basedir]) $
		error "cp failed"
#else
installSkel topdir _basedir = do
	whenM (doesDirectoryExist topdir) $
		removeDirectoryRecursive topdir
	createDirectoryIfMissing True (takeDirectory topdir)
	unlessM (boolSystem "cp" [Param "-R", File "standalone/linux/skel", File topdir]) $
		error "cp failed"
#endif

installSkelRest :: FilePath -> FilePath -> Bool -> IO ()
#ifdef darwin_HOST_OS
installSkelRest topdir basedir _hwcaplibs = do
	plist <- lines <$> readFile "standalone/osx/Info.plist.template"
	version <- getVersion
	writeFile (basedir </> "Contents" </> "Info.plist")
		(unlines (map (expandversion version) plist))
  where
	expandversion v l = replace "GIT_ANNEX_VERSION" v l
#else
installSkelRest topdir _basedir hwcaplibs = do
	runshell <- lines <$> readFile "standalone/linux/skel/runshell"
	-- GIT_ANNEX_PACKAGE_INSTALL can be set by a distributor and
	-- runshell will be modified
	gapi <- getEnv "GIT_ANNEX_PACKAGE_INSTALL"
	writeFile (topdir </> "runshell")
		(unlines (map (expandrunshell gapi) runshell))
	modifyFileMode (topdir </> "runshell") (addModes executeModes)
  where
	expandrunshell (Just gapi) l@"GIT_ANNEX_PACKAGE_INSTALL=" = l ++ gapi
	-- This is an optimisation, that avoids the linker looking in
	-- several directories for hwcap optimised libs, when there are
	-- none.
	expandrunshell _ l@"LD_HWCAP_MASK=" = l ++ if not hwcaplibs
		then "0"
		else ""
	expandrunshell _ l = l
#endif

installGitAnnex :: FilePath -> IO ()
#ifdef darwin_HOST_OS
installGitAnnex topdir = go topdir
#else
installGitAnnex topdir = go (topdir </> "bin")
#endif
  where
	go bindir = do
		createDirectoryIfMissing True bindir
		unlessM (boolSystem "cp" [File "git-annex", File bindir]) $
			error "cp failed"
		unlessM (boolSystem "strip" [File (bindir </> "git-annex")]) $
			error "strip failed"
		createSymbolicLink "git-annex" (bindir </> "git-annex-shell")
		createSymbolicLink "git-annex" (bindir </> "git-remote-tor-annex")

main :: IO ()
main = getArgs >>= go
  where
	go (topdir:basedir:[]) = do
		installSkel topdir basedir
		installGitAnnex topdir
		installedbins <- installBundledPrograms topdir
		installGitLibs topdir
		installMagic topdir
		installLocales topdir
		hwcaplibs <- mklibs topdir installedbins
		installSkelRest topdir basedir hwcaplibs
	go _ = error "specify topdir and basedir"
