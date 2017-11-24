{- Calculating a SHA checksum with an external command.
 -
 - This is typically a bit faster than using Haskell libraries,
 - by around 1% to 10%. Worth it for really big files.
 -
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.ExternalSHA (externalSHA) where

import Utility.SafeCommand
import Utility.Process
import Utility.Misc
import Utility.Exception

import Data.List
import Data.Char
import System.IO

externalSHA :: String -> Int -> FilePath -> IO (Either String String)
externalSHA command shasize file = do
	v <- tryNonAsync $ readsha $ toCommand [File file]
	return $ case v of
		Right s -> sanitycheck =<< parse (lines s)
		Left _ -> Left (command ++ " failed")
  where
	readsha args = withHandle StdoutHandle createProcessSuccess p $ \h -> do
		output  <- hGetContentsStrict h
		hClose h
		return output
	  where
		p = (proc command args) { std_out = CreatePipe }

	{- The first word of the output is taken to be the sha. -}
	parse [] = bad
	parse (l:_)
		| null sha = bad
		-- sha is prefixed with \ when filename contains certian chars
		| "\\" `isPrefixOf` sha = Right $ drop 1 sha
		| otherwise = Right sha
	  where
		sha = fst $ separate (== ' ') l
	bad = Left $ command ++ " parse error"

	{- Check that we've correctly parsing the output of the command,
	 - by making sure the sha we read is of the expected length
	 - and contains only the right characters. -}
	sanitycheck sha
		| length sha /= expectedSHALength shasize =
			Left $ "Failed to parse the output of " ++ command
		| any (`notElem` "0123456789abcdef") sha' =
			Left $ "Unexpected character in output of " ++ command ++ "\"" ++ sha ++ "\""
		| otherwise = Right sha'
	  where
		sha' = map toLower sha

expectedSHALength :: Int -> Int
expectedSHALength 1 = 40
expectedSHALength 256 = 64
expectedSHALength 512 = 128
expectedSHALength 224 = 56
expectedSHALength 384 = 96
expectedSHALength _ = 0
