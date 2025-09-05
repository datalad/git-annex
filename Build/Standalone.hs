{- Makes standalone bundle.
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Control.Monad.IfElse
import System.Posix.Files
import Control.Monad
import qualified Data.Map as M

import Utility.SafeCommand
import Utility.Process
import Utility.OsPath
import Utility.Path
import Utility.Path.AbsRel
import Utility.Directory
import Utility.Env
import Utility.SystemDirectory
import qualified Utility.FileIO as F
import Build.BundledPrograms
#ifdef darwin_HOST_OS
import System.IO
import Build.OSXMkLibs (mklibs)
import Build.Version
import Utility.Split
#else
import Build.LinuxMkLibs (mklibs)
import Utility.FileMode
#endif

progDir :: OsPath -> OsPath
#ifdef darwin_HOST_OS
progDir topdir = topdir
#else
progDir topdir = topdir </> literalOsPath "bin"
#endif

extraProgDir :: OsPath -> OsPath
extraProgDir topdir = topdir </> "extra"

installProg :: OsPath -> OsPath -> IO (OsPath, OsPath)
installProg dir prog = searchPath (fromOsPath prog) >>= go
  where
	go Nothing = error $ "cannot find " ++ fromOsPath prog ++ " in PATH"
	go (Just f) = do
		let dest = dir </> takeFileName f
		unlessM (boolSystem "install" [File (fromOsPath f), File (fromOsPath dest)]) $
			error $ "install failed for " ++ fromOsPath prog
		return (dest, f)

installBundledPrograms :: OsPath -> IO (M.Map OsPath OsPath)
installBundledPrograms topdir = M.fromList . concat <$> mapM go
	[ (progDir topdir, map toOsPath preferredBundledPrograms)
	, (extraProgDir topdir, map toOsPath extraBundledPrograms)
	]
  where
	go (dir, progs) = do
		createDirectoryIfMissing True dir
		forM progs $ installProg dir

installGitLibs :: OsPath -> IO ()
installGitLibs topdir = do
	-- install git-core programs; these are run by the git command
	createDirectoryIfMissing True gitcoredestdir
	execpath <- getgitpath "exec-path"
	cfs <- dirContents execpath
	forM_ cfs $ \f -> do
		let f' = fromOsPath f
		destf <- (gitcoredestdir </>)
			<$> relPathDirToFile execpath f
		createDirectoryIfMissing True (takeDirectory destf)
		issymlink <- isSymbolicLink <$> getSymbolicLinkStatus f'
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
				linktarget <- toOsPath <$> readSymbolicLink f'
				if takeFileName linktarget == linktarget
					then cp f destf
					else do
						let linktarget' = progDir topdir </> takeFileName linktarget
						unlessM (doesFileExist linktarget') $ do
							createDirectoryIfMissing True (takeDirectory linktarget')
							F.readFile f >>= F.writeFile linktarget'
						removeWhenExistsWith removeFile destf
						rellinktarget <- relPathDirToFile
							(takeDirectory destf)
							(linktarget')
						createSymbolicLink (fromOsPath rellinktarget) (fromOsPath destf)
			else cp f destf
	
	-- install git's template files
	-- git does not have an option to get the path of these,
	-- but they're architecture independent files, so are located
	-- next to the --man-path, in eg /usr/share/git-core
	manpath <- getgitpath "man-path"
	let templatepath = manpath </> literalOsPath ".." </> literalOsPath "git-core" </> literalOsPath "templates"
	tfs <- dirContents templatepath
	forM_ tfs $ \f -> do
		destf <- (templatedestdir </>)
			<$> relPathDirToFile templatepath f
		createDirectoryIfMissing True (takeDirectory destf)
		cp f destf
  where
	gitcoredestdir = topdir </> literalOsPath "git-core"
	templatedestdir = topdir </> literalOsPath "templates"

	getgitpath v = do
		let opt = "--" ++ v
		ls <- lines <$> readProcess "git" [opt]
		case ls of
			[] -> error $ "git " ++ opt ++ "did not output a location"
			(p:_) -> return (toOsPath p)

cp :: OsPath -> OsPath -> IO ()
cp src dest = do
	removeWhenExistsWith removeFile dest
	unlessM (boolSystem "cp" [Param "-a", File (fromOsPath src), File (fromOsPath dest)]) $
		error "cp failed"

installMagic :: OsPath -> IO ()
#ifdef darwin_HOST_OS
installMagic topdir = getEnv "OSX_MAGIC_FILE" >>= \case
	Nothing -> hPutStrLn stderr "OSX_MAGIC_FILE not set; not including it"
	Just f -> do
		let mdir = topdir </> literalOsPath "magic"
		createDirectoryIfMissing True mdir
		unlessM (boolSystem "cp" [File f, File (fromOsPath (mdir </> literalOsPath "magic.mgc"))]) $
			error "cp failed"
#else
installMagic topdir = do
	let mdir = topdir </> literalOsPath "magic"
	createDirectoryIfMissing True mdir
	unlessM (boolSystem "cp" [File "/usr/share/file/magic.mgc", File (fromOsPath (mdir </> literalOsPath "magic.mgc"))]) $
		error "cp failed"
#endif

installLocales :: OsPath -> IO ()
#ifdef darwin_HOST_OS
installLocales _ = return ()
#else
installLocales topdir =
	cp (literalOsPath "/usr/share/i18n") (topdir </> "i18n")
#endif

installSkel :: OsPath -> OsPath -> IO ()
#ifdef darwin_HOST_OS
installSkel _topdir basedir = do
	whenM (doesDirectoryExist basedir) $
		removeDirectoryRecursive basedir
	createDirectoryIfMissing True (takeDirectory basedir)
	unlessM (boolSystem "cp" [Param "-R", File "standalone/osx/git-annex.app", File (fromOsPath basedir)]) $
		error "cp failed"
#else
installSkel topdir _basedir = do
	whenM (doesDirectoryExist topdir) $
		removeDirectoryRecursive topdir
	createDirectoryIfMissing True (takeDirectory topdir)
	unlessM (boolSystem "cp" [Param "-R", File "standalone/linux/skel", File (fromOsPath topdir)]) $
		error "cp failed"
#endif

installSkelRest :: OsPath -> OsPath -> Bool -> IO ()
#ifdef darwin_HOST_OS
installSkelRest _topdir basedir _hwcaplibs = do
	plist <- lines <$> F.readFileString (literalOsPath "standalone/osx/Info.plist.template")
	version <- getVersion
	F.writeFileString (basedir </> literalOsPath "Contents" </> literalOsPath "Info.plist")
		(unlines (map (expandversion version) plist))
  where
	expandversion v l = replace "GIT_ANNEX_VERSION" v l
#else
installSkelRest topdir _basedir hwcaplibs = do
	runshell <- lines <$> F.readFileString (literalOsPath "standalone/linux/skel/runshell")
	-- GIT_ANNEX_PACKAGE_INSTALL can be set by a distributor and
	-- runshell will be modified
	gapi <- getEnv "GIT_ANNEX_PACKAGE_INSTALL"
	F.writeFileString (topdir </> literalOsPath "runshell")
		(unlines (map (expandrunshell gapi) runshell))
	modifyFileMode
		(topdir </> literalOsPath "runshell")
		(addModes executeModes)
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

installGitAnnex :: OsPath -> IO ()
#ifdef darwin_HOST_OS
installGitAnnex topdir = go topdir
#else
installGitAnnex topdir = go (topdir </> literalOsPath "bin")
#endif
  where
	go bindir = do
		createDirectoryIfMissing True bindir
		unlessM (boolSystem "cp" [File "git-annex", File (fromOsPath bindir)]) $
			error "cp failed"
		unlessM (boolSystem "strip" [File (fromOsPath (bindir </> literalOsPath "git-annex"))]) $
			error "strip failed"
		createSymbolicLink "git-annex" (fromOsPath (bindir </> literalOsPath "git-annex-shell"))
		createSymbolicLink "git-annex" (fromOsPath (bindir </> literalOsPath "git-remote-p2p-annex"))
		createSymbolicLink "git-annex" (fromOsPath (bindir </> literalOsPath "git-remote-tor-annex"))
		createSymbolicLink "git-annex" (fromOsPath (bindir </> literalOsPath "git-remote-annex"))

main :: IO ()
main = getArgs >>= go . map toOsPath
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
