{- Generates a NullSoft installer program for git-annex on Windows.
 - 
 - git-annex should already be built by cabal, and ssh and rsync,
 - as well as cygwin libraries, already installed.
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

main = writeFile "git-annex.nsi" $ nsis $ do
	name "git-annex"
	outFile "git-annex-installer.exe"
	installDir "$DESKTOP/git-annex"
	requestExecutionLevel User
	iff_ (fileExists "$WINDIR/git.exe")
		(return ()) $ do
		messageBox [MB_ABORTRETRYIGNORE]
			"git does not seem to be installed. git-annex can't be used without git!"
	-- Pages to display
	page Directory                   -- Pick where to install
	page InstFiles                   -- Give a progress bar while installing
	-- Groups of files to install
	section "programs" [] $ do
		setOutPath "$INSTDIR"
		file [] "dist/build/git-annex/git-annex.exe"
		fromcygwin "rsync.exe"
		fromcygwin "ssh.exe"
		fromcygwin "sha256.exe"
		fromcygwin "sha1.exe"
		fromcygwin "sha512.exe"
		fromcygwin "sha384.exe"
	section "DLLS" [] $ mapM_ fromcygwin 
		setOutPath "$INSTDIR"
		[ "cygwin1.dll"
		, "cygasn1-8.dll"
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
  where
	fromcygwin f = file [] (str $ "/bin/" ++ f)
