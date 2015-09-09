{- gpg interface
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Gpg where

import Common
import qualified Build.SysConfig as SysConfig
#ifndef mingw32_HOST_OS
import System.Posix.Types
import qualified System.Posix.IO
import System.Path
import Utility.Env
#else
import Utility.Tmp
#endif
import Utility.Format (decode_c)

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Map as M

type KeyId = String

newtype KeyIds = KeyIds { keyIds :: [KeyId] }
	deriving (Ord, Eq)

newtype GpgCmd = GpgCmd { unGpgCmd :: String }

{- Get gpg command to use, Just what's specified or, if a specific gpg
 - command was found at configure time, use it, or otherwise, "gpg". -}
mkGpgCmd :: Maybe FilePath -> GpgCmd
mkGpgCmd (Just c) = GpgCmd c
mkGpgCmd Nothing = GpgCmd (fromMaybe "gpg" SysConfig.gpg)

boolGpgCmd :: GpgCmd -> [CommandParam] -> IO Bool
boolGpgCmd (GpgCmd cmd) = boolSystem cmd

-- Generate an argument list to asymetrically encrypt to the given recipients.
pkEncTo :: [String] -> [CommandParam]
pkEncTo = concatMap (\r -> [Param "--recipient", Param r])

stdParams :: [CommandParam] -> IO [String]
stdParams params = do
#ifndef mingw32_HOST_OS
	-- Enable batch mode if GPG_AGENT_INFO is set, to avoid extraneous
	-- gpg output about password prompts. GPG_BATCH is set by the test
	-- suite for a similar reason.
	e <- getEnv "GPG_AGENT_INFO"
	b <- getEnv "GPG_BATCH"
	let batch = if isNothing e && isNothing b
		then []
		else ["--batch", "--no-tty", "--use-agent"]
	return $ batch ++ defaults ++ toCommand params
#else
	return $ defaults ++ toCommand params
#endif
  where
	-- Be quiet, even about checking the trustdb.
	defaults = ["--quiet", "--trust-model", "always"]

{- Usual options for symmetric / public-key encryption. -}
stdEncryptionParams :: Bool -> [CommandParam]
stdEncryptionParams symmetric = enc symmetric ++
	[ Param "--force-mdc"
	, Param "--no-textmode"
	]
  where
	enc True = [ Param "--symmetric" ]
	-- Force gpg to only encrypt to the specified recipients, not
	-- configured defaults. Recipients are assumed to be specified in
	-- elsewhere.
	enc False =
		[ Param "--encrypt"
		, Param "--no-encrypt-to"
		, Param "--no-default-recipient"
		]

{- Runs gpg with some params and returns its stdout, strictly. -}
readStrict :: GpgCmd -> [CommandParam] -> IO String
readStrict (GpgCmd cmd) params = do
	params' <- stdParams params
	withHandle StdoutHandle createProcessSuccess (proc cmd params') $ \h -> do
		hSetBinaryMode h True
		hGetContentsStrict h

{- Runs gpg, piping an input value to it, and returning its stdout,
 - strictly. -}
pipeStrict :: GpgCmd -> [CommandParam] -> String -> IO String
pipeStrict (GpgCmd cmd) params input = do
	params' <- stdParams params
	withIOHandles createProcessSuccess (proc cmd params') $ \(to, from) -> do
		hSetBinaryMode to True
		hSetBinaryMode from True
		hPutStr to input
		hClose to
		hGetContentsStrict from

{- Runs gpg with some parameters. First sends it a passphrase (unless it
 - is empty) via '--passphrase-fd'. Then runs a feeder action that is
 - passed a handle and should write to it all the data to input to gpg.
 - Finally, runs a reader action that is passed a handle to gpg's
 - output.
 -
 - Runs gpg in batch mode; this is necessary to avoid gpg 2.x prompting for
 - the passphrase.
 -
 - Note that to avoid deadlock with the cleanup stage,
 - the reader must fully consume gpg's input before returning. -}
feedRead :: (MonadIO m, MonadMask m) => GpgCmd -> [CommandParam] -> String -> (Handle -> IO ()) -> (Handle -> m a) -> m a
feedRead cmd params passphrase feeder reader = do
#ifndef mingw32_HOST_OS
	-- pipe the passphrase into gpg on a fd
	(frompipe, topipe) <- liftIO System.Posix.IO.createPipe
	liftIO $ void $ forkIO $ do
		toh <- fdToHandle topipe
		hPutStrLn toh passphrase
		hClose toh
	let Fd pfd = frompipe
	let passphrasefd = [Param "--passphrase-fd", Param $ show pfd]
	liftIO (closeFd frompipe) `after` go (passphrasefd ++ params)
#else
	-- store the passphrase in a temp file for gpg
	withTmpFile "gpg" $ \tmpfile h -> do
		liftIO $ hPutStr h passphrase
		liftIO $ hClose h
		let passphrasefile = [Param "--passphrase-file", File tmpfile]
		go $ passphrasefile ++ params
#endif
  where
	go params' = pipeLazy cmd params' feeder reader

{- Like feedRead, but without passphrase. -}
pipeLazy :: (MonadIO m, MonadMask m) => GpgCmd -> [CommandParam] -> (Handle -> IO ()) -> (Handle -> m a) -> m a
pipeLazy (GpgCmd cmd) params feeder reader = do
	params' <- liftIO $ stdParams $ Param "--batch" : params
	let p = (proc cmd params')
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		}
	bracket (setup p) (cleanup p) go
  where
	setup = liftIO . createProcess
	cleanup p (_, _, _, pid) = liftIO $ forceSuccessProcess p pid
	go p = do
		let (to, from) = ioHandles p
		liftIO $ void $ forkIO $ do
			feeder to
			hClose to
		reader from

{- Finds gpg public keys matching some string. (Could be an email address,
 - a key id, or a name; See the section 'HOW TO SPECIFY A USER ID' of
 - GnuPG's manpage.) -}
findPubKeys :: GpgCmd -> String -> IO KeyIds
findPubKeys cmd for = KeyIds . parse . lines <$> readStrict cmd params
  where
	params = [Param "--with-colons", Param "--list-public-keys", Param for]
	parse = mapMaybe (keyIdField . split ":")
	keyIdField ("pub":_:_:_:f:_) = Just f
	keyIdField _ = Nothing

type UserId = String

{- All of the user's secret keys, with their UserIds.
 - Note that the UserId may be empty. -}
secretKeys :: GpgCmd -> IO (M.Map KeyId UserId)
secretKeys cmd = catchDefaultIO M.empty makemap
  where
	makemap = M.fromList . parse . lines <$> readStrict cmd params
	params = [Param "--with-colons", Param "--list-secret-keys", Param "--fixed-list-mode"]
	parse = extract [] Nothing . map (split ":")
	extract c (Just keyid) (("uid":_:_:_:_:_:_:_:_:userid:_):rest) =
		extract ((keyid, decode_c userid):c) Nothing rest
	extract c (Just keyid) rest =
		extract ((keyid, ""):c) Nothing rest
	extract c _ [] = c
	extract c _ (("sec":_:_:_:keyid:_):rest) =
		extract c (Just keyid) rest
	extract c k (_:rest) =
		extract c k rest

type Passphrase = String
type Size = Int
data KeyType = Algo Int | DSA | RSA

{- The maximum key size that gpg currently offers in its UI when
 - making keys. -}
maxRecommendedKeySize :: Size
maxRecommendedKeySize = 4096

{- Generates a secret key using the experimental batch mode.
 - The key is added to the secret key ring.
 - Can take a very long time, depending on system entropy levels.
 -}
genSecretKey :: GpgCmd -> KeyType -> Passphrase -> UserId -> Size -> IO ()
genSecretKey (GpgCmd cmd) keytype passphrase userid keysize =
	withHandle StdinHandle createProcessSuccess (proc cmd params) feeder
  where
	params = ["--batch", "--gen-key"]
	feeder h = do
		hPutStr h $ unlines $ catMaybes
			[ Just $  "Key-Type: " ++ 
				case keytype of
					DSA -> "DSA"
					RSA -> "RSA"
					Algo n -> show n
			, Just $ "Key-Length: " ++ show keysize
			, Just $ "Name-Real: " ++ userid
			, Just "Expire-Date: 0"
			, if null passphrase
				then Nothing
				else Just $ "Passphrase: " ++ passphrase
			]
		hClose h

{- Creates a block of high-quality random data suitable to use as a cipher.
 - It is armored, to avoid newlines, since gpg only reads ciphers up to the
 - first newline. -}
genRandom :: GpgCmd -> Bool -> Size -> IO String
genRandom cmd highQuality size = checksize <$> readStrict cmd params
  where
	params = 
		[ Param "--gen-random"
		, Param "--armor"
		, Param $ show randomquality
		, Param $ show size
		]

	-- See http://www.gnupg.org/documentation/manuals/gcrypt/Quality-of-random-numbers.html
	-- for the meaning of random quality levels.
	-- The highest available is 2, which is the default for OpenPGP
	-- key generation; Note that it uses the blocking PRNG /dev/random
	-- on the Linux kernel, hence the running time may take a while.
	randomquality :: Int
	randomquality = if highQuality then 2 else 1

	{- The size is the number of bytes of entropy desired; the data is
	 - base64 encoded, so needs 8 bits to represent every 6 bytes of
	 - entropy. -}
	expectedlength = size * 8 `div` 6

	checksize s = let len = length s in
		if len >= expectedlength
			then s
			else shortread len

	shortread got = error $ unwords
		[ "Not enough bytes returned from gpg", show params
		, "(got", show got, "; expected", show expectedlength, ")"
		]

{- A test key. This is provided pre-generated since generating a new gpg
 - key is too much work (requires too much entropy) for a test suite to
 - do.
 -
 - This key was generated with no exipiration date, and a small keysize. 
 - It has an empty passphrase. -}
testKeyId :: String
testKeyId = "129D6E0AC537B9C7"
testKey :: String
testKey = keyBlock True
	[ "mI0ETvFAZgEEAKnqwWgZqznMhi1RQExem2H8t3OyKDxaNN3rBN8T6LWGGqAYV4wT"
	, "r8In5tfsnz64bKpE1Qi68JURFwYmthgUL9N48tbODU8t3xzijdjLOSaTyqkH1ik6"
	, "EyulfKN63xLne9i4F9XqNwpiZzukXYbNfHkDA2yb0M6g4UFKLY/fNzGXABEBAAG0"
	, "W2luc2VjdXJlIHRlc3Qga2V5ICh0aGlzIGlzIGEgdGVzdCBrZXksIGRvIG5vdCB1"
	, "c2UgZm9yIGFjdHVhbCBlbmNyeXB0aW9uKSA8dGVzdEBleGFtcGxlLmNvbT6IuAQT"
	, "AQgAIgUCTvFAZgIbAwYLCQgHAwIGFQgCCQoLBBYCAwECHgECF4AACgkQEp1uCsU3"
	, "uceQ9wP/YMd1f0+/eLLcwGXNBvGqyVhUOfAKknO1bMzGbqTsq9g60qegy/cldqee"
	, "xVxNfy0VN//JeMfgdcb8+RgJYLoaMrTy9CcsUcFPxtwN9tcLmsM0V2/fNmmFBO9t"
	, "v75iH+zeFbNg0/FbPkHiN6Mjw7P2gXYKQXgTvQZBWaphk8oQlBm4jQRO8UBmAQQA"
	, "vdi50M/WRCkOLt2RsUve8V8brMWYTJBJTTWoHUeRr82v4NCdX7OE1BsoVK8cy/1Q"
	, "Y+gLOH9PqinuGGNWRmPV2Ju/RYn5H7sdewXA8E80xWhc4phHRMJ8Jjhg/GVPamkJ"
	, "8B5zeKF0jcLFl7cuVdOyQakhoeDWJd0CyfW837nmPtMAEQEAAYifBBgBCAAJBQJO"
	, "8UBmAhsMAAoJEBKdbgrFN7nHclAEAKBShuP/toH03atDUQTbGE34CA4yEC9BVghi"
	, "7kviOZlOz2s8xAfp/8AYsrECx1kgbXcA7JD902eNyp7NzXsdJX0zJwHqiuZW0XlD"
	, "T8ZJu4qrYRYgl/790WPESZ+ValvHD/fqkR38RF4tfxvyoMhhp0roGmJY33GASIG/"
	, "+gQkDF9/"
	, "=1k11"
	]
testSecretKey :: String
testSecretKey = keyBlock False
	[ "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"
	, "=LDsg"
	]
keyBlock :: Bool -> [String] -> String
keyBlock public ls = unlines
	[ "-----BEGIN PGP "++t++" KEY BLOCK-----"
	, "Version: GnuPG v1.4.11 (GNU/Linux)"
	, ""
	, unlines ls
	, "-----END PGP "++t++" KEY BLOCK-----"
	]
  where
	t
		| public = "PUBLIC"
		| otherwise = "PRIVATE"

#ifndef mingw32_HOST_OS
{- Runs an action using gpg in a test harness, in which gpg does
 - not use ~/.gpg/, but a directory with the test key set up to be used. -}
testHarness :: GpgCmd -> IO a -> IO a
testHarness cmd a = do
	orig <- getEnv var
	bracket setup (cleanup orig) (const a)
  where
	var = "GNUPGHOME"		

	setup = do
		base <- getTemporaryDirectory
		dir <- mktmpdir $ base </> "gpgtmpXXXXXX"
		setEnv var dir True
		-- For some reason, recent gpg needs a trustdb to be set up.
		_ <- pipeStrict cmd [Param "--trust-model", Param "auto", Param "--update-trustdb"] []
		_ <- pipeStrict cmd [Param "--import", Param "-q"] $ unlines
			[testSecretKey, testKey]
		return dir
		
	cleanup orig tmpdir = removeDirectoryRecursive tmpdir >> reset orig
	reset (Just v) = setEnv var v True
	reset _ = unsetEnv var

{- Tests the test harness. -}
testTestHarness :: GpgCmd -> IO Bool
testTestHarness cmd = do
	keys <- testHarness cmd $ findPubKeys cmd testKeyId
	return $ KeyIds [testKeyId] == keys
#endif

#ifndef mingw32_HOST_OS
checkEncryptionFile :: GpgCmd -> FilePath -> Maybe KeyIds -> IO Bool
checkEncryptionFile cmd filename keys =
	checkGpgPackets cmd keys =<< readStrict cmd params
  where
	params = [Param "--list-packets", Param "--list-only", File filename]

checkEncryptionStream :: GpgCmd -> String -> Maybe KeyIds -> IO Bool
checkEncryptionStream cmd stream keys =
	checkGpgPackets cmd keys =<< pipeStrict cmd params stream
  where
	params = [Param "--list-packets", Param "--list-only"]

{- Parses an OpenPGP packet list, and checks whether data is
 - symmetrically encrypted (keys is Nothing), or encrypted to some
 - public key(s).
 - /!\ The key needs to be in the keyring! -}
checkGpgPackets :: GpgCmd -> Maybe KeyIds -> String -> IO Bool
checkGpgPackets cmd keys str = do
	let (asym,sym) = partition (pubkeyEncPacket `isPrefixOf`) $
			filter (\l' -> pubkeyEncPacket `isPrefixOf` l' ||
				symkeyEncPacket `isPrefixOf` l') $
			takeWhile (/= ":encrypted data packet:") $
			lines str
	case (keys,asym,sym) of
		(Nothing, [], [_]) -> return True
		(Just (KeyIds ks), ls, []) -> do
			-- Find the master key associated with the
			-- encryption subkey.
			ks' <- concat <$> mapM (keyIds <$$> findPubKeys cmd)
					[ k | k:"keyid":_ <- map (reverse . words) ls ]
			return $ sort (nub ks) == sort (nub ks')
		_ -> return False
  where
	pubkeyEncPacket = ":pubkey enc packet: "
	symkeyEncPacket = ":symkey enc packet: "
#endif
