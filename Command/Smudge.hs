{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Smudge where

import Common.Annex
import Command
import Types.Key

import qualified Data.ByteString.Lazy as B

cmd :: Command
cmd = dontCheck repoExists $
	command "smudge" SectionPlumbing 
		"git smudge filter"
		paramFile (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [_file] = do
	liftIO $ fileEncoding stdin
	s <- liftIO $ hGetContents stdin
	case parsePointer s of
		Nothing -> liftIO $ putStr s
		Just k -> do
			content <- calcRepo (gitAnnexLocation k)
			liftIO $ maybe
				(putStr s)
				(B.hPut stdout)
				=<< catchMaybeIO (B.readFile content)
	stop
start [] = error "smudge filter run without filename; upgrade git"
start _ = error "smudge filter passed multiple filenames"

parsePointer :: String -> Maybe Key
parsePointer s
	| length s' >= maxsz = Nothing -- too long to be a key pointer
	| otherwise = headMaybe (lines s') >>= file2key
  where
	s' = take maxsz s
	maxsz = 81920
