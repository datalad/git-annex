{- gpg interface
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Utility.Gpg (
	KeyId,
	KeyIds(..),
	GpgCmd(..),
	mkGpgCmd,
	boolGpgCmd,
	pkEncTo,
	stdEncryptionParams,
	pipeStrict,
	pipeStrict',
	feedRead,
	feedRead',
	findPubKeys,
	UserId,
	secretKeys,
	genSecretKey,
	genRandom,
	testKeyId,
#ifndef mingw32_HOST_OS
	testHarness,
	checkEncryptionFile,
	checkEncryptionStream,
#endif
) where

import Common
import qualified BuildInfo
#ifndef mingw32_HOST_OS
import System.Posix.Types
import System.Posix.IO
import Utility.Env
import Utility.FileMode
#else
import Utility.Tmp
#endif
import Utility.Format (decode_c)

import Control.Concurrent.Async
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Char

type KeyId = String

newtype KeyIds = KeyIds { keyIds :: [KeyId] }
	deriving (Ord, Eq)

newtype GpgCmd = GpgCmd { unGpgCmd :: String }

type Passphrase = B.ByteString

{- Get gpg command to use, Just what's specified or, if a specific gpg
 - command was found at configure time, use it, or otherwise, "gpg". -}
mkGpgCmd :: Maybe FilePath -> GpgCmd
mkGpgCmd (Just c) = GpgCmd c
mkGpgCmd Nothing = GpgCmd (fromMaybe "gpg" BuildInfo.gpg)

boolGpgCmd :: GpgCmd -> [CommandParam] -> IO Bool
boolGpgCmd (GpgCmd cmd) = boolSystem cmd

-- Generate an argument list to asymmetrically encrypt to the given recipients.
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
readStrict :: GpgCmd -> [CommandParam] -> IO B.ByteString
readStrict c p = readStrict' c p Nothing

readStrict' :: GpgCmd -> [CommandParam] -> Maybe [(String, String)] -> IO B.ByteString
readStrict' (GpgCmd cmd) params environ = do
	params' <- stdParams params
	let p = (proc cmd params')
		{ std_out = CreatePipe
		, env = environ
		}
	withCreateProcess p (go p)
  where
	go p _ (Just hout) _ pid =
		forceSuccessProcess p pid `after` B.hGetContents hout
	go _ _ _ _ _ = error "internal"

{- Runs gpg, piping an input value to it, and returning its stdout,
 - strictly. -}
pipeStrict :: GpgCmd -> [CommandParam] -> B.ByteString -> IO B.ByteString
pipeStrict c p i = pipeStrict' c p Nothing i

pipeStrict' :: GpgCmd -> [CommandParam] -> Maybe [(String, String)] -> B.ByteString -> IO B.ByteString
pipeStrict' (GpgCmd cmd) params environ input = do
	params' <- stdParams params
	let p = (proc cmd params')
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, env = environ
		}
	withCreateProcess p (go p)
  where
	go p (Just to) (Just from) _ pid = do
		B.hPutStr to input
		hClose to
		forceSuccessProcess p pid `after` B.hGetContents from
	go _ _ _ _ _ = error "internal"

{- Runs gpg with some parameters. First sends it a passphrase via
 - '--passphrase-fd'. Then runs a feeder action that is
 - passed a handle and should write to it all the data to input to gpg.
 - Finally, runs a reader action that is passed a handle to gpg's
 - output.
 -
 - Runs gpg in batch mode; this is necessary to avoid gpg 2.x prompting for
 - the passphrase.
 -
 - Note that the reader must fully consume gpg's input before returning. -}
feedRead :: (MonadIO m, MonadMask m) => GpgCmd -> [CommandParam] -> Passphrase -> (Handle -> IO ()) -> (Handle -> m a) -> m a
feedRead cmd params passphrase feeder reader = do
#ifndef mingw32_HOST_OS
	let setup = liftIO $ do
		-- pipe the passphrase into gpg on a fd
		(frompipe, topipe) <- System.Posix.IO.createPipe
		setFdOption topipe CloseOnExec True
		toh <- fdToHandle topipe
		t <- async $ do
			B.hPutStr toh (passphrase <> "\n")
			hClose toh
		let Fd pfd = frompipe
		let passphrasefd = [Param "--passphrase-fd", Param $ show pfd]
		return (passphrasefd, frompipe, toh, t)
	let cleanup (_, frompipe, toh, t) = liftIO $ do
		closeFd frompipe
		hClose toh
		cancel t
	bracket setup cleanup $ \(passphrasefd, _, _, _) -> 
		go (passphrasefd ++ params)
#else
	-- store the passphrase in a temp file for gpg
	withTmpFile (toOsPath "gpg") $ \tmpfile h -> do
		liftIO $ B.hPutStr h passphrase
		liftIO $ hClose h
		let passphrasefile = [Param "--passphrase-file", File (fromRawFilePath (fromOsPath tmpfile))]
		go $ passphrasefile ++ params
#endif
  where
	go params' = feedRead' cmd params' feeder reader

{- Like feedRead, but without passphrase. -}
feedRead' :: (MonadIO m, MonadMask m) => GpgCmd -> [CommandParam] -> (Handle -> IO ()) -> (Handle -> m a) -> m a
feedRead' (GpgCmd cmd) params feeder reader = do
	params' <- liftIO $ stdParams $ Param "--batch" : params
	let p = (proc cmd params')
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		}
	bracket (setup p) cleanup (go p)
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

{- Finds gpg public keys matching some string. (Could be an email address,
 - a key id, or a name; See the section 'HOW TO SPECIFY A USER ID' of
 - GnuPG's manpage.) -}
findPubKeys :: GpgCmd -> String -> IO KeyIds
findPubKeys cmd = findPubKeys' cmd Nothing

findPubKeys' :: GpgCmd -> Maybe [(String, String)] -> String -> IO KeyIds
findPubKeys' cmd environ for
	-- pass forced subkey through as-is rather than
	-- looking up the master key.
	| isForcedSubKey for = return $ KeyIds [for]
	| otherwise = KeyIds . parse . lines . decodeBS
		<$> readStrict' cmd params environ
  where
	params = [Param "--with-colons", Param "--list-public-keys", Param for]
	parse = mapMaybe (keyIdField . splitc ':')
	keyIdField ("pub":_:_:_:f:_) = Just f
	keyIdField _ = Nothing

{- "subkey!" tells gpg to force use of a specific subkey -}
isForcedSubKey :: String -> Bool
isForcedSubKey s = "!" `isSuffixOf` s && all isHexDigit (drop 1 (reverse s))

type UserId = String

{- All of the user's secret keys, with their UserIds.
 - Note that the UserId may be empty. -}
secretKeys :: GpgCmd -> IO (M.Map KeyId UserId)
secretKeys cmd = catchDefaultIO M.empty makemap
  where
	makemap = M.fromList . parse . lines . decodeBS
		<$> readStrict cmd params
	params = [Param "--with-colons", Param "--list-secret-keys", Param "--fixed-list-mode"]
	parse = extract [] Nothing . map (splitc ':')
	extract c (Just keyid) (("uid":_:_:_:_:_:_:_:_:userid:_):rest) =
		-- If the userid contains a ":" or a few other special
		-- characters, gpg will hex-escape it. Use decode_c to
		-- undo.
		extract ((keyid, decodeBS (decode_c (encodeBS userid))):c) Nothing rest
	extract c (Just keyid) rest@(("sec":_):_) =
		extract ((keyid, ""):c) Nothing rest
	extract c (Just keyid) (_:rest) =
		extract c (Just keyid) rest
	extract c _ [] = c
	extract c _ (("sec":_:_:_:keyid:_):rest) =
		extract c (Just keyid) rest
	extract c k (_:rest) =
		extract c k rest

{- Generates a secret key.
 - The key is added to the secret key ring.
 - Can take a very long time, depending on system entropy levels.
 -}
genSecretKey :: GpgCmd -> Passphrase -> UserId -> IO ()
genSecretKey gpgcmd passphrase userid =
	feedRead gpgcmd params passphrase feeder reader
  where
	params =
		[ Param "--batch"
		, Param "--quick-gen-key"
		, Param userid
		, Param "default" -- algo
		, Param "default" -- usage
		, Param "never" -- expire
		]
	feeder = hClose
	reader = void . hGetContents

{- Creates a block of high-quality random data suitable to use as a cipher.
 - It is armored, to avoid newlines, since gpg only reads ciphers up to the
 - first newline. -}
genRandom :: GpgCmd -> Bool -> Int -> IO B.ByteString
genRandom cmd highQuality size = do
	s <- readStrict cmd params
	checksize s
	return s
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

	checksize s = let len = B.length s in
		unless (len >= expectedlength) $
			shortread len

	shortread got = giveup $ unwords
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

#ifndef mingw32_HOST_OS
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

{- Runs an action using gpg in a test harness, in which gpg does
 - not use ~/.gpg/, but sets up the test key in a subdirectory of 
 - the passed directory and uses it.
 -
 - If the test harness is not able to be set up (eg, because gpg is not
 - installed or because there is some problem importing the test key,
 - perhaps related to the agent socket), the action is not run, and Nothing
 - is returned.
 -}
testHarness :: FilePath -> GpgCmd -> ([(String, String)] -> IO a) -> IO (Maybe a)
testHarness tmpdir cmd a = ifM (inSearchPath (unGpgCmd cmd))
	( bracket (eitherToMaybe <$> tryNonAsync setup) cleanup go
	, return Nothing
	)
  where
	var = "GNUPGHOME"

	setup = do
		subdir <- makenewdir (1 :: Integer)
		origenviron <- getEnvironment
		let environ = addEntry var subdir origenviron
		-- gpg is picky about permissions on its home dir
		liftIO $ void $ tryIO $ modifyFileMode (toRawFilePath subdir) $
			removeModes $ otherGroupModes
		-- For some reason, recent gpg needs a trustdb to be set up.
		_ <- pipeStrict' cmd [Param "--trust-model", Param "auto", Param "--update-trustdb"] (Just environ) mempty
		_ <- pipeStrict' cmd [Param "--import", Param "-q"] (Just environ) $ encodeBS $ unlines
			[testSecretKey, testKey]
		return environ
		
	cleanup Nothing = return ()
	cleanup (Just environ) = stopgpgagent environ

	-- Recent versions of gpg automatically start gpg-agent, or perhaps
	-- other daemons. Stop them when done. This only affects
	-- daemons started for the GNUPGHOME that was used.
	-- Older gpg may not support this, so ignore failure.
	stopgpgagent environ = whenM (inSearchPath "gpgconf") $
		void $ boolSystemEnv "gpgconf" [Param "--kill", Param "all"]
			(Just environ) 

	go (Just environ) = Just <$> a environ
	go Nothing = return Nothing

        makenewdir n = do
		let subdir = tmpdir </> show n
		catchIOErrorType AlreadyExists (const $ makenewdir $ n + 1) $ do
			createDirectory subdir
			return subdir

checkEncryptionFile :: GpgCmd -> Maybe [(String, String)] -> FilePath -> Maybe KeyIds -> IO Bool
checkEncryptionFile cmd environ filename keys =
	checkGpgPackets cmd environ keys =<< readStrict' cmd params environ
  where
	params = [Param "--list-packets", Param "--list-only", File filename]

checkEncryptionStream :: GpgCmd -> Maybe [(String, String)] -> B.ByteString -> Maybe KeyIds -> IO Bool
checkEncryptionStream cmd environ stream keys =
	checkGpgPackets cmd environ keys =<< pipeStrict' cmd params environ stream
  where
	params = [Param "--list-packets", Param "--list-only"]

{- Parses an OpenPGP packet list, and checks whether data is
 - symmetrically encrypted (keys is Nothing), or encrypted to some
 - public key(s).
 - /!\ The key needs to be in the keyring! -}
checkGpgPackets :: GpgCmd -> Maybe [(String, String)] -> Maybe KeyIds -> B.ByteString -> IO Bool
checkGpgPackets cmd environ keys str = do
	let (asym,sym) = partition (pubkeyEncPacket `isPrefixOf`) $
			filter (\l' -> pubkeyEncPacket `isPrefixOf` l' ||
				symkeyEncPacket `isPrefixOf` l') $
			takeWhile (/= ":encrypted data packet:") $
			lines (decodeBS str)
	case (keys,asym,sym) of
		(Nothing, [], [_]) -> return True
		(Just (KeyIds ks), ls, []) -> do
			-- Find the master key associated with the
			-- encryption subkey.
			ks' <- concat <$> mapM (keyIds <$$> findPubKeys' cmd environ)
					[ k | k:"keyid":_ <- map (reverse . words) ls ]
			return $ sort (nub ks) == sort (nub ks')
		_ -> return False
  where
	pubkeyEncPacket = ":pubkey enc packet: "
	symkeyEncPacket = ":symkey enc packet: "
#endif
