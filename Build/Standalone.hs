{- Makes standalone bundle.
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.IfElse
import System.Environment
import System.FilePath
import System.Posix.Files
import Control.Monad
import Build.BundledPrograms
import qualified Data.ByteString.Lazy as L

import Utility.SafeCommand
import Utility.Process
import Utility.Path
import Utility.Directory

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
						let linktarget' = progDir topdir
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
	
	cp src dest = do
		nukeFile dest
		unlessM (boolSystem "cp" [Param "-a", File src, File dest]) $
			error "cp failed"

main :: IO ()
main = getArgs >>= go
  where
	go [] = error "specify topdir"
	go (topdir:_) = do
		installed <- forM
			[ (progDir topdir, preferredBundledPrograms)
			, (extraProgDir topdir, extraBundledPrograms) ] $ \(dir, progs) -> do
			createDirectoryIfMissing True dir
			forM progs $ installProg dir
		writeFile "tmp/standalone-installed" (show (concat installed))
		installGitLibs topdir
