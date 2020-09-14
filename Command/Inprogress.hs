{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Inprogress where

import Command
import Annex.Transfer

import qualified Data.Set as S

cmd :: Command
cmd = noCommit $ noMessages $ command "inprogress" SectionQuery
		"access files while they're being downloaded"
		paramPaths (seek <$$> optParser)

data InprogressOptions = InprogressOptions
	{ inprogressFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	}

optParser :: CmdParamsDesc -> Parser InprogressOptions
optParser desc = InprogressOptions
	<$> cmdParams desc
	<*> optional (parseAllOption <|> parseSpecificKeyOption)

seek :: InprogressOptions -> CommandSeek
seek o = do
	ts <- map (transferKey . fst) <$> getTransfers
	case keyOptions o of
		Just WantAllKeys ->
			forM_ ts $ commandAction . start'
		Just (WantSpecificKey k)
			| k `elem` ts -> commandAction (start' k)
			| otherwise -> commandAction stop
		_ -> do
			let s = S.fromList ts
			let seeker = AnnexedFileSeeker
				{ startAction = start s
				, checkContentPresent = Nothing
				, usesLocationLog = False
				}
			withFilesInGitAnnex ww seeker
				=<< workTreeItems ww (inprogressFiles o)
  where
	ww = WarnUnmatchLsFiles

start :: S.Set Key -> SeekInput -> RawFilePath -> Key -> CommandStart
start s _si _file k
	| S.member k s = start' k
	| otherwise = stop

start' :: Key -> CommandStart
start' k = startingCustomOutput k $ do
	tmpf <- fromRepo $ gitAnnexTmpObjectLocation k
	whenM (liftIO $ doesFileExist tmpf) $
		liftIO $ putStrLn tmpf
	next $ return True
