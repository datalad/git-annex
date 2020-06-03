{- Magic Wormhole integration
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.MagicWormhole (
	Code,
	mkCode,
	toCode,
	fromCode,
	validCode,
	CodeObserver,
	CodeProducer,
	mkCodeObserver,
	mkCodeProducer,
	waitCode,
	sendCode,
	WormHoleParams,
	appId,
	sendFile,
	receiveFile,
	isInstalled,
) where

import Utility.Process
import Utility.SafeCommand
import Utility.Monad
import Utility.Misc
import Utility.Env
import Utility.Path
import Utility.Exception

import System.IO
import System.Exit
import Control.Concurrent
import Control.Concurrent.Async
import Data.Char
import Data.List
import Control.Applicative
import Prelude

-- | A Magic Wormhole code.
newtype Code = Code String
	deriving (Eq, Show)

-- | Smart constructor for Code
mkCode :: String -> Maybe Code
mkCode s
	| validCode s = Just (Code s)
	| otherwise = Nothing

-- | Tries to fix up some common mistakes in a homan-entered code.
toCode :: String -> Maybe Code
toCode s = mkCode $ intercalate "-" $ words s

fromCode :: Code -> String
fromCode (Code s) = s

-- | Codes have the form number-word-word and may contain 2 or more words.
validCode :: String -> Bool
validCode s = 
	let (n, r) = separate (== '-') s
	    (w1, w2) = separate (== '-') r
	in and
		[ not (null n)
		, all isDigit n
		, not (null w1)
		, not (null w2)
		, not $ any isSpace s
		]

newtype CodeObserver = CodeObserver (MVar Code)

newtype CodeProducer = CodeProducer (MVar Code)

mkCodeObserver :: IO CodeObserver
mkCodeObserver = CodeObserver <$> newEmptyMVar

mkCodeProducer :: IO CodeProducer
mkCodeProducer = CodeProducer <$> newEmptyMVar

waitCode :: CodeObserver -> IO Code
waitCode (CodeObserver o) = readMVar o

sendCode :: CodeProducer -> Code -> IO ()
sendCode (CodeProducer p) = putMVar p

type WormHoleParams = [CommandParam]

-- | An appid should be provided when using wormhole in an app, to avoid
-- using the same channel space as ad-hoc wormhole users.
appId :: String -> WormHoleParams
appId s = [Param "--appid", Param s]

-- | Sends a file. Once the send is underway, and the Code has been
-- generated, it will be sent to the CodeObserver. (This may not happen,
-- eg if there's a network problem).
--
-- Currently this has to parse the output of wormhole to find the code.
-- To make this as robust as possible, avoids looking for any particular
-- output strings, and only looks for the form of a wormhole code
-- (number-word-word). 
--
-- Note that, if the filename looks like "foo 1-wormhole-code bar", when
-- that is output by wormhole, it will look like it's output a wormhole code.
--
-- A request to make the code available in machine-parsable form is here:
-- https://github.com/warner/magic-wormhole/issues/104
sendFile :: FilePath -> CodeObserver -> WormHoleParams -> IO Bool
sendFile f (CodeObserver observer) ps = do
	-- Work around stupid stdout buffering behavior of python.
	-- See https://github.com/warner/magic-wormhole/issues/108
	environ <- addEntry "PYTHONUNBUFFERED" "1" <$> getEnvironment
	runWormHoleProcess p { env = Just environ} $ \_hin hout herr -> do
		(inout, inerr) <- findcode hout `concurrently` findcode herr
		return (inout || inerr)
  where
	p = wormHoleProcess (Param "send" : ps ++ [File f])
	findcode h = findcode' =<< words <$> hGetContents h
	findcode' [] = return False
	findcode' (w:ws) = case mkCode w of
		Just code -> do
			_ <- tryPutMVar observer code
			return True
		Nothing -> findcode' ws

-- | Receives a file. Once the receive is under way, the Code will be
-- read from the CodeProducer, and fed to wormhole on stdin.
receiveFile :: FilePath -> CodeProducer -> WormHoleParams -> IO Bool
receiveFile f (CodeProducer producer) ps = runWormHoleProcess p $ \hin _hout _herr -> do
	Code c <- readMVar producer
	hPutStrLn hin c
	hFlush hin
	return True
  where
	p = wormHoleProcess $
		[ Param "receive"
		, Param "--accept-file"
		, Param "--output-file"
		, File f
		] ++ ps

wormHoleProcess :: WormHoleParams -> CreateProcess
wormHoleProcess = proc "wormhole" . toCommand

runWormHoleProcess :: CreateProcess -> (Handle -> Handle -> Handle -> IO Bool) ->  IO Bool
runWormHoleProcess p consumer =
	withCreateProcess p' go `catchNonAsync` const (return False)
  where
	p' = p 
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = CreatePipe
		}
	go (Just hin) (Just hout) (Just herr) pid =
		consumer hin hout herr <&&> waitbool pid
	go _ _ _ _ = error "internal"
	waitbool pid = do
		r <- waitForProcess pid
		return $ case r of
			ExitSuccess -> True
			ExitFailure _ -> False

isInstalled :: IO Bool
isInstalled = inPath "wormhole"
