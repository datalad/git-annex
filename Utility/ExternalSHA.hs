{- Calculating a SHA checksum with an external command.
 -
 - This is typically a bit faster than using Haskell libraries,
 - by around 1% to 10%. Worth it for really big files.
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

module Utility.ExternalSHA (externalSHA) where

import Utility.SafeCommand
import Utility.Process
import Utility.FileSystemEncoding
import Utility.Misc

import System.Process
import Data.List
import Data.Char
import Control.Applicative
import System.IO

externalSHA :: String -> Int -> FilePath -> IO (Either String String)
externalSHA command shasize file = do
	ls <- lines <$> readsha (toCommand [File file])
	return $ sanitycheck =<< parse ls
  where
	{- sha commands output the filename, so need to set fileEncoding -}
	readsha args =
		withHandle StdoutHandle (createProcessChecked checkSuccessProcess) p $ \h -> do
			fileEncoding h
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
