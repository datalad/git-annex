{- Generates a NullSoft installer program for git-annex on Windows.
 - 
 - To build the installer, git-annex should already be built by cabal,
 - and ssh and rsync, as well as cygwin libraries, already installed.
 -
 - This uses the Haskell nsis package (cabal install nsis)
 - to generate a .nsi file, which is then used to produce
 - git-annex-installer.exe
 - 
 - The installer includes git-annex, and utilities it uses, with the
 - exception of git. The user needs to install git separately,
 - and the installer checks for that.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

import Development.NSIS
import System.Directory
import System.FilePath
import Control.Monad
import Data.String
import Data.Maybe

import Utility.Tmp
import Utility.Path
import Utility.CopyFile
import Utility.SafeCommand
import Build.BundledPrograms

main = do
	withTmpDir "nsis-build" $ \tmpdir -> do
		let gitannex = tmpdir </> gitannexprogram
		mustSucceed "ln" [File "dist/build/git-annex/git-annex.exe", File gitannex]
		let license = tmpdir </> licensefile
		mustSucceed "sh" [Param "-c", Param $ "zcat standalone/licences.gz > '" ++ license ++ "'"]
		extrafiles <- forM (cygwinPrograms ++ cygwinDlls) $ \f -> do
			p <- searchPath f
			when (isNothing p) $
				print ("unable to find in PATH", f)
			return p
		writeFile nsifile $ makeInstaller gitannex license $
			catMaybes extrafiles
		mustSucceed "makensis" [File nsifile]
	removeFile nsifile -- left behind if makensis fails
  where
	nsifile = "git-annex.nsi"
	mustSucceed cmd params = do
		r <- boolSystem cmd params
		case r of
			True -> return ()
			False -> error $ cmd ++ " failed"

gitannexprogram :: FilePath
gitannexprogram = "git-annex.exe"

licensefile :: FilePath
licensefile = "git-annex-licenses.txt"

installer :: FilePath
installer = "git-annex-installer.exe"

uninstaller :: FilePath
uninstaller = "git-annex-uninstall.exe"

gitInstallDir :: Exp FilePath
gitInstallDir = fromString "$PROGRAMFILES\\Git\\cmd"

startMenuItem :: Exp FilePath
startMenuItem = "$SMPROGRAMS/git-annex.lnk"

needGit :: Exp String
needGit = strConcat
	[ fromString "You need git installed to use git-annex. Looking at "
	, gitInstallDir
	, fromString " , it seems to not be installed, "
	, fromString "or may be installed in another location. "
	, fromString "You can install git from http:////git-scm.com//"
	]

makeInstaller :: FilePath -> FilePath -> [FilePath] -> String
makeInstaller gitannex license extrafiles = nsis $ do
	name "git-annex"
	outFile $ str installer
	{- Installing into the same directory as git avoids needing to modify
 	 - path myself, since the git installer already does it. -}
	installDir gitInstallDir
	requestExecutionLevel Admin

	iff (fileExists gitInstallDir)
		(return ())
		(alert needGit)
	
	-- Pages to display
	page Directory                   -- Pick where to install
	page (License license)
	page InstFiles                   -- Give a progress bar while installing
	-- Start menu shortcut
	Development.NSIS.createDirectory "$SMPROGRAMS"
	createShortcut startMenuItem
		[ Target "$INSTDIR/git-annex.exe"
		, Parameters "webapp"
		, IconFile "$INSTDIR/git-annex.exe"
		, IconIndex 2
		, StartOptions "SW_SHOWMINIMIZED"
		, KeyboardShortcut "ALT|CONTROL|a"
		, Description "git-annex webapp"
		]
	-- Groups of files to install
	section "main" [] $ do
		setOutPath "$INSTDIR"
		addfile gitannex
		addfile license
		mapM_ addfile extrafiles
		writeUninstaller $ str uninstaller
	uninstall $ do
		delete [RebootOk] $ startMenuItem
		mapM_ (\f -> delete [RebootOK] $ fromString $ "$INSTDIR/" ++ f) $
			[ gitannexprogram
			, licensefile
			, uninstaller
			] ++ cygwinPrograms ++ cygwinDlls
  where
	addfile f = file [] (str f)

cygwinPrograms :: [FilePath]
cygwinPrograms = map (\p -> p ++ ".exe") bundledPrograms

-- These are the dlls needed by Cygwin's rsync, ssh, etc.
cygwinDlls :: [FilePath]
cygwinDlls =
	[ "cygwin1.dll"
	, "cygasn1-8.dll"
	, "cygattr-1.dll"
	, "cygheimbase-1.dll"
	, "cygroken-18.dll"
	, "cygcom_err-2.dll"
	, "cygheimntlm-0.dll"
	, "cygsqlite3-0.dll"
	, "cygcrypt-0.dll"
	, "cyghx509-5.dll"
	, "cygssp-0.dll"
	, "cygcrypto-1.0.0.dll"
	, "cygiconv-2.dll"
	, "cyggcc_s-1.dll"
	, "cygintl-8.dll"
	, "cygwind-0.dll"
	, "cyggssapi-3.dll"
	, "cygkrb5-26.dll"
	, "cygz.dll"
	]
