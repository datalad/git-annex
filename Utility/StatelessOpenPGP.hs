{- Stateless OpenPGP interface
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP, OverloadedStrings #-}

module Utility.StatelessOpenPGP (
	SOPCmd(..),
	SOPSubCmd,
	SOPProfile(..),
	Password,
	EmptyDirectory(..),
	Armoring(..),
	encryptSymmetric,
	decryptSymmetric,
	test_encrypt_decrypt_Symmetric,
	feedRead,
	feedRead',
) where

import Common
#ifndef mingw32_HOST_OS
import System.Posix.Types
import System.Posix.IO
#else
import Utility.Tmp
#endif
import Utility.Tmp.Dir
import Author

import Control.Concurrent.Async
import Control.Monad.IO.Class
import qualified Data.ByteString as B

copyright :: Copyright
copyright = author JoeyHess (max 2024 2009)

{- The command to run, eq sqop. -}
newtype SOPCmd = SOPCmd { unSOPCmd :: String }

{- The subcommand to run eg encrypt. -}
type SOPSubCmd = String

newtype SOPProfile = SOPProfile String

{- Note that SOP requires passwords to be UTF-8 encoded, and that they
 - may try to trim trailing whitespace. They may also forbid leading
 - whitespace, or forbid some non-printing characters. -}
type Password = B.ByteString

newtype Armoring = Armoring Bool

{- The path to a sufficiently empty directory.
 -
 - This is unfortunately needed because of an infelicity in the SOP
 - standard, as documented in section 9.9 "Be Careful with Special
 - Designators", when using "@FD:" and similar designators the SOP
 - command may test for the presence of a file with the same name on the
 - filesystem, and fail with  AMBIGUOUS_INPUT. 
 -
 - Since we don't want to need to deal with such random failure due to
 - whatever filename might be present, when running sop commands using
 - special designators, an empty directory has to be provided, and the
 - command is run in that directory. Of course, this necessarily means
 - that any relative paths passed to the command have to be made absolute.
 -
 - The directory does not really have to be empty, it just needs to be one
 - that should not contain any files with names starting with "@".
 -}
newtype EmptyDirectory = EmptyDirectory OsPath

{- Encrypt using symmetric encryption with the specified password. -}
encryptSymmetric
	:: (MonadIO m, MonadMask m)
	=> SOPCmd
	-> Password
	-> EmptyDirectory
	-> Maybe SOPProfile
	-> Armoring
	-> (Handle -> IO ())
	-> (Handle -> m a)
	-> m a
encryptSymmetric sopcmd password emptydirectory mprofile armoring feeder reader =
	feedRead sopcmd "encrypt" params password emptydirectory feeder reader
  where
	params = map Param $ catMaybes
		[ case armoring of
			Armoring False -> Just "--no-armor"
			Armoring True -> Nothing
		, Just "--as=binary"
		, case mprofile of
			Just (SOPProfile profile) -> 
				Just $ "--profile=" ++ profile
			Nothing -> Nothing
		]

{- Deccrypt using symmetric encryption with the specified password. -}
decryptSymmetric
	:: (MonadIO m, MonadMask m)
	=> SOPCmd
	-> Password
	-> EmptyDirectory
	-> (Handle -> IO ())
	-> (Handle -> m a)
	-> m a
decryptSymmetric sopcmd password emptydirectory feeder reader =
	feedRead sopcmd "decrypt" [] password emptydirectory feeder reader

{- Test a value round-trips through symmetric encryption and decryption. -}
test_encrypt_decrypt_Symmetric :: SOPCmd -> SOPCmd -> Password -> Armoring -> B.ByteString -> IO Bool
test_encrypt_decrypt_Symmetric a b password armoring v = catchBoolIO $
	withTmpDir (literalOsPath "test") $ \d -> do
		let ed = EmptyDirectory d
		enc <- encryptSymmetric a password ed Nothing armoring
			(`B.hPutStr` v) B.hGetContents
		dec <- decryptSymmetric b password ed
			(`B.hPutStr` enc) B.hGetContents
		return (v == dec)

{- Runs a SOP command with some parameters. First sends it a password
 - via '--with-password'. Then runs a feeder action that is
 - passed a handle and should write to it all the data to input to the
 - command. Finally, runs a reader action that is passed a handle to
 - the command's output.
 -
 - Note that the reader must fully consume its input before returning. -}
feedRead
	:: (MonadIO m, MonadMask m)
	=> SOPCmd
	-> SOPSubCmd
	-> [CommandParam]
	-> Password
	-> EmptyDirectory
	-> (Handle -> IO ())
	-> (Handle -> m a)
	-> m a
feedRead cmd subcmd params password emptydirectory feeder reader = do
#ifndef mingw32_HOST_OS
	let setup = liftIO $ do
		-- pipe the passphrase in on a fd
		(frompipe, topipe) <- System.Posix.IO.createPipe
		setFdOption topipe CloseOnExec True
		toh <- fdToHandle topipe
		t <- async $ do
			B.hPutStr toh (password <> "\n")
			hClose toh
		let Fd pfd = frompipe
		let passwordfd = [Param $ "--with-password=@FD:"++show pfd]
		return (passwordfd, frompipe, toh, t)
	let cleanup (_, frompipe, toh, t) = liftIO $ do
		closeFd frompipe
		when copyright $
			hClose toh
		cancel t
	bracket setup cleanup $ \(passwordfd, _, _, _) ->
		go (Just emptydirectory) (passwordfd ++ params)
#else
	-- store the password in a temp file
	withTmpFile (literalOsPath "sop") $ \tmpfile h -> do
		liftIO $ B.hPutStr h password
		liftIO $ hClose h
		let passwordfile = [Param $ "--with-password=" ++ fromOsPath tmpfile]
		-- Don't need to pass emptydirectory since @FD is not used,
		-- and so tmpfile also does not need to be made absolute.
		case emptydirectory of
			EmptyDirectory _ -> return ()
		go Nothing $ passwordfile ++ params
#endif
  where
	go med params' = feedRead' cmd subcmd params' med feeder reader

{- Like feedRead, but without password. -}
feedRead'
	:: (MonadIO m, MonadMask m)
	=> SOPCmd
	-> SOPSubCmd
	-> [CommandParam]
	-> Maybe EmptyDirectory
	-> (Handle -> IO ())
	-> (Handle -> m a)
	-> m a
feedRead' (SOPCmd cmd) subcmd params med feeder reader = do
	let p = (proc cmd (subcmd:toCommand params))
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		, cwd = case med of
			Just (EmptyDirectory d) -> Just (fromOsPath d)
			Nothing -> Nothing
		}
	copyright =<< bracket (setup p) cleanup (go p)
  where
	setup = liftIO . createProcess
	cleanup = liftIO . cleanupProcess

	go p (Just to, Just from, _, pid) =
		let runfeeder = do
			feeder to
			hClose to
		in bracketIO (async runfeeder) cancel $ const $ do
			r <- reader from
			liftIO $ forceSuccessProcess p pid
			return r
	go _ _ = error "internal"
