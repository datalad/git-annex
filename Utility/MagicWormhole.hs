{- Magic Wormhole integration
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.MagicWormHole where

import Utility.Process
import Utility.SafeCommand
import Utility.Monad
import Utility.Misc
import Utility.FileSystemEncoding
import Utility.Env

import System.IO
import System.Exit
import Control.Concurrent
import Control.Exception
import Data.Char

-- | A Magic Wormhole code.
type Code = String

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

type CodeObserver = MVar Code

type WormHoleParams = [CommandParam]

mkCodeObserver :: IO CodeObserver
mkCodeObserver = newEmptyMVar

waitCode :: CodeObserver -> IO Code
waitCode = takeMVar

sendCode :: CodeObserver -> Code -> IO ()
sendCode = putMVar

-- | Sends a file. Once the send is underway, the Code will be sent to the
-- CodeObserver.
--
-- Currently this has to parse the output of wormhole to find the code.
-- To make this as robust as possible, avoids looking for any particular
-- output strings, and only looks for the form of a wormhole code
-- (number-word-word).
--
-- A request to make the code available in machine-parsable form is here:
-- https://github.com/warner/magic-wormhole/issues/104
sendFile :: FilePath -> CodeObserver -> WormHoleParams -> IO Bool
sendFile f o ps = do
	-- Work around stupid stdout buffering behavior of python.
	-- See https://github.com/warner/magic-wormhole/issues/108
	environ <- addEntry "PYTHONUNBUFFERED" "1" <$> getEnvironment
	runWormHoleProcess p { env = Just environ} $ \_hin hout -> do
		fileEncoding hout
		findcode =<< words <$> hGetContents hout
  where
	p = wormHoleProcess (Param "send" : ps ++ [File f])
	findcode [] = return False
	findcode (w:ws)
		| validCode w = do
			sendCode o w
			return True
		| otherwise = findcode ws

-- | Receives a file. Once the receive is under way, the Code will be
-- read from the CodeObserver, and fed to it on stdin.
receiveFile :: FilePath -> CodeObserver -> WormHoleParams -> IO Bool
receiveFile f o ps = runWormHoleProcess p $ \hin hout -> do
	hPutStrLn hin =<< waitCode o
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

runWormHoleProcess :: CreateProcess -> (Handle -> Handle -> IO Bool) ->  IO Bool
runWormHoleProcess p consumer = bracketOnError setup cleanup go
  where
	setup = do
		(Just hin, Just hout, Nothing, pid)
			<- createProcess p
				{ std_in = CreatePipe
				, std_out = CreatePipe
				}
		return (hin, hout, pid)
	cleanup (hin, hout, pid) = do
		r <- waitForProcess pid
		hClose hin
		hClose hout
		return $ case r of
			ExitSuccess -> True
			ExitFailure _ -> False
	go h@(hin, hout, _) = consumer hin hout <&&> cleanup h
