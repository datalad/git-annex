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
	, allOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser InprogressOptions
optParser desc = InprogressOptions
	<$> cmdParams desc
	<*> switch
		( long "all"
		<> short 'A'
		<> help "access all files currently being downloaded"
		)

seek :: InprogressOptions -> CommandSeek
seek o = do
	ts <- map (transferKey . fst) <$> getTransfers
	if allOption o
		then forM_ ts $ commandAction . start'
		else do
			let s = S.fromList ts
			withFilesInGit
				(commandAction . (whenAnnexed (start s)))
				=<< workTreeItems (inprogressFiles o)

start :: S.Set Key -> RawFilePath -> Key -> CommandStart
start s _file k
	| S.member k s = start' k
	| otherwise = stop

start' :: Key -> CommandStart
start' k = startingCustomOutput k $ do
	tmpf <- fromRepo $ gitAnnexTmpObjectLocation k
	whenM (liftIO $ doesFileExist tmpf) $
		liftIO $ putStrLn tmpf
	next $ return True
