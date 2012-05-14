{- gpg interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Gpg where

import qualified Data.ByteString.Lazy.Char8 as L
import System.Posix.Types
import Control.Applicative
import Control.Concurrent
import Control.Exception (finally, bracket)
import System.Exit
import System.Posix.Env (setEnv, unsetEnv, getEnv)	

import Common

newtype KeyIds = KeyIds [String]
        deriving (Ord, Eq)

stdParams :: [CommandParam] -> IO [String]
stdParams params = do
	-- Enable batch mode if GPG_AGENT_INFO is set, to avoid extraneous
	-- gpg output about password prompts. GPG_BATCH is set by the test
	-- suite for a similar reason.
	e <- getEnv "GPG_AGENT_INFO"
	b <- getEnv "GPG_BATCH"
	let batch = if isNothing e && isNothing b
		then []
		else ["--batch", "--no-tty"]
	return $ batch ++ defaults ++ toCommand params
	where
		-- be quiet, even about checking the trustdb
		defaults = ["--quiet", "--trust-model", "always"]

{- Runs gpg with some params and returns its stdout, strictly. -}
readStrict :: [CommandParam] -> IO String
readStrict params = do
	params' <- stdParams params
	pOpen ReadFromPipe "gpg" params' hGetContentsStrict

{- Runs gpg, piping an input value to it, and returning its stdout,
 - strictly. -}
pipeStrict :: [CommandParam] -> String -> IO String
pipeStrict params input = do
	params' <- stdParams params
	(pid, fromh, toh) <- hPipeBoth "gpg" params'
	_ <- forkIO $ finally (hPutStr toh input) (hClose toh)
	output <- hGetContentsStrict fromh
	forceSuccess pid
	return output

{- Runs gpg with some parameters, first feeding it a passphrase via
 - --passphrase-fd, then feeding it an input, and passing a handle
 - to its output to an action.
 -
 - Note that to avoid deadlock with the cleanup stage,
 - the action must fully consume gpg's input before returning. -}
passphraseHandle :: [CommandParam] -> String -> IO L.ByteString -> (Handle -> IO a) -> IO a
passphraseHandle params passphrase a b = do
	-- pipe the passphrase into gpg on a fd
	(frompipe, topipe) <- createPipe
	_ <- forkIO $ do
		toh <- fdToHandle topipe
		hPutStrLn toh passphrase
		hClose toh
	let Fd pfd = frompipe
	let passphrasefd = [Param "--passphrase-fd", Param $ show pfd]

	params' <- stdParams $ passphrasefd ++ params
	(pid, fromh, toh) <- hPipeBoth "gpg" params'
	pid2 <- forkProcess $ do
		L.hPut toh =<< a
		hClose toh
		exitSuccess
	hClose toh
	ret <- b fromh

	-- cleanup
	forceSuccess pid
	_ <- getProcessStatus True False pid2
	closeFd frompipe
	return ret

{- Finds gpg public keys matching some string. (Could be an email address,
 - a key id, or a name. -}
findPubKeys :: String -> IO KeyIds
findPubKeys for = KeyIds . parse <$> readStrict params
	where
		params = [Params "--with-colons --list-public-keys", Param for]
		parse = map keyIdField . filter pubKey . lines
		pubKey = isPrefixOf "pub:"
		keyIdField s = split ":" s !! 4

{- Creates a block of high-quality random data suitable to use as a cipher.
 - It is armored, to avoid newlines, since gpg only reads ciphers up to the
 - first newline. -}
genRandom :: Int -> IO String
genRandom size = readStrict
	[ Params "--gen-random --armor"
	, Param $ show randomquality
	, Param $ show size
	]
	where
		-- 1 is /dev/urandom; 2 is /dev/random
		randomquality = 1 :: Int

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
	[ "lQHYBE7xQGYBBACp6sFoGas5zIYtUUBMXpth/Ldzsig8WjTd6wTfE+i1hhqgGFeM"
	, "E6/CJ+bX7J8+uGyqRNUIuvCVERcGJrYYFC/TePLWzg1PLd8c4o3Yyzkmk8qpB9Yp"
	, "OhMrpXyjet8S53vYuBfV6jcKYmc7pF2GzXx5AwNsm9DOoOFBSi2P3zcxlwARAQAB"
	, "AAP+PlRboxy7Z0XjuG70N6+CrzSddQbW5KCwgPFrxYsPk7sAPFcBkmRMVlv9vZpS"
	, "phbP4bvDK+MrSntM51g+9uE802yhPhSWdmEbImiWfV2ucEhlLjD8gw7JDex9XZ0a"
	, "EbTOV56wOsILuedX/jF/6i6IQzy5YmuMeo+ip1XQIsIN+80CAMyXepOBJgHw/gBD"
	, "VdXh/l//vUkQQlhInQYwgkKbr0POCTdr8DM1qdKLcUD9Q1khgNRp0vZGGz+5xsrc"
	, "KaODUlMCANSczLJcYWa8yPqB3S14yTe7qmtDiOS362+SeVUwQA7eQ06PcHLPsN+p"
	, "NtWoHRfYazxrs+g0JvmoQOYdj4xSQy0CAMq7H/l6aeG1n8tpyMxqE7OvBOsvzdu5"
	, "XS7I1AnwllVFgvTadVvqgf7b+hdYd91doeHDUGqSYO78UG1GgaBHJdylqrRbaW5z"
	, "ZWN1cmUgdGVzdCBrZXkgKHRoaXMgaXMgYSB0ZXN0IGtleSwgZG8gbm90IHVzZSBm"
	, "b3IgYWN0dWFsIGVuY3J5cHRpb24pIDx0ZXN0QGV4YW1wbGUuY29tPoi4BBMBCAAi"
	, "BQJO8UBmAhsDBgsJCAcDAgYVCAIJCgsEFgIDAQIeAQIXgAAKCRASnW4KxTe5x5D3"
	, "A/9gx3V/T794stzAZc0G8arJWFQ58AqSc7VszMZupOyr2DrSp6DL9yV2p57FXE1/"
	, "LRU3/8l4x+B1xvz5GAlguhoytPL0JyxRwU/G3A321wuawzRXb982aYUE722/vmIf"
	, "7N4Vs2DT8Vs+QeI3oyPDs/aBdgpBeBO9BkFZqmGTyhCUGZ0B2ARO8UBmAQQAvdi5"
	, "0M/WRCkOLt2RsUve8V8brMWYTJBJTTWoHUeRr82v4NCdX7OE1BsoVK8cy/1QY+gL"
	, "OH9PqinuGGNWRmPV2Ju/RYn5H7sdewXA8E80xWhc4phHRMJ8Jjhg/GVPamkJ8B5z"
	, "eKF0jcLFl7cuVdOyQakhoeDWJd0CyfW837nmPtMAEQEAAQAD/RaVtFFTkF1udun7"
	, "YOwzJvQXCO9OWHZvSdEeG4BUNdAwy4YWu0oZzKkBDBS6+lWILqqb/c28U4leUJ1l"
	, "H+viz5svN9BWWyj/UpI00uwUo9JaIqalemwfLx6vsh69b54L1B4exLZHYGLvy/B3"
	, "5T6bT0gpOE+53BRtKcJaOh/McQeJAgDTOCBU5weWOf6Bhqnw3Vr/gRfxntAz2okN"
	, "gqz/h79mWbCc/lHKoYQSsrCdMiwziHSjXwvehUrdWE/AcomtW0vbAgDmGJqJ2fNr"
	, "HvdsGx4Ld/BxyiZbCURJLUQ5CwzfHGIvBu9PMT8zM26NOSncaXRjxDna2Ggh8Uum"
	, "ANEwbnhxFwZpAf9L9RLYIMTtAqwBjfXJg/lHcc2R+VP0hL5c8zFz+S+w7bRqINwL"
	, "ff1JstKuHT2nJnu0ustK66by8YI3T0hDFFahnNCInwQYAQgACQUCTvFAZgIbDAAK"
	, "CRASnW4KxTe5x3JQBACgUobj/7aB9N2rQ1EE2xhN+AgOMhAvQVYIYu5L4jmZTs9r"
	, "PMQH6f/AGLKxAsdZIG13AOyQ/dNnjcqezc17HSV9MycB6ormVtF5Q0/GSbuKq2EW"
	, "IJf+/dFjxEmflWpbxw/36pEd/EReLX8b8qDIYadK6BpiWN9xgEiBv/oEJAxffw=="
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
 - not use ~/.gpg/, but a directory with the test key set up to be used. -}
testHarness :: IO a -> IO a
testHarness a = do
	orig <- getEnv var
	bracket setup (cleanup orig) (const a)
	where
		var = "GNUPGHOME"		

		setup = do
			base <- getTemporaryDirectory
			dir <- mktmpdir $ base </> "gpgtmpXXXXXX"
			setEnv var dir True
			_ <- pipeStrict [Params "--import -q"] $ unlines
				[testSecretKey, testKey]
			return dir
		
		cleanup orig tmpdir = removeDirectoryRecursive tmpdir >> reset orig
                reset (Just v) = setEnv var v True
                reset _ = unsetEnv var

{- Tests the test harness. -}
testTestHarness :: IO Bool
testTestHarness = do
	keys <- testHarness $ findPubKeys testKeyId
	return $ KeyIds [testKeyId] == keys
