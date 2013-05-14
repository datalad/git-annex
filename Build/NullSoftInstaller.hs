{- Generates a NullSoft installer program for git-annex on Windows.
 - 
 - git-annex should already be built by cabal, and ssh and rsync,
 - as well as cygwin libraries, already installed.
 -}

{-# LANGUAGE OverloadedStrings #-}

import Development.NSIS

main = writeFile "git-annex.nsi" $ nsis $ do
	name "git-annex"
	outFile "git-annex.exe"
	installDir "$DESKTOP/git-annex"
	requestExecutionLevel User
	-- Pages to display
	page Directory                   -- Pick where to install
	page InstFiles                   -- Give a progress bar while installing
	-- Groups of files to install
	section "programs" [] $ do
		setOutPath "$INSTDIR"
		file [] "dist/build/git-annex/git-annex.exe"
		fromcygwin "rsync.exe"
		fromcygwin "ssh.exe"
	section "DLLS" [] $ mapM_ fromcygwin 
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
