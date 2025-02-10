{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Inprogress where

import Command
import Annex.Transfer
import Utility.Terminal
import Utility.SafeOutput

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
	isterminal <- liftIO $ checkIsTerminal stdout
	ts <- map (transferKey . fst) <$> getTransfers
	case keyOptions o of
		Just WantAllKeys ->
			forM_ ts $ commandAction . (start' isterminal)
		Just (WantSpecificKey k)
			| k `elem` ts -> commandAction (start' isterminal k)
			| otherwise -> commandAction stop
		_ -> do
			let s = S.fromList ts
			let seeker = AnnexedFileSeeker
				{ startAction = const $ start isterminal s
				, checkContentPresent = Nothing
				, usesLocationLog = False
				}
			withFilesInGitAnnex ww seeker
				=<< workTreeItems ww (inprogressFiles o)
  where
	ww = WarnUnmatchLsFiles "inprogress"

start :: IsTerminal -> S.Set Key -> SeekInput -> OsPath -> Key -> CommandStart
start isterminal s _si _file k
	| S.member k s = start' isterminal k
	| otherwise = stop

start' :: IsTerminal -> Key -> CommandStart
start' (IsTerminal isterminal) k = startingCustomOutput k $ do
	tmpf <- fromRepo (gitAnnexTmpObjectLocation k)
	whenM (liftIO $ doesFileExist tmpf) $
		liftIO $ putStrLn $ 
			if isterminal
				then safeOutput (fromOsPath tmpf)
				else fromOsPath tmpf
	next $ return True
