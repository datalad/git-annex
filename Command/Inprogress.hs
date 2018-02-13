{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
			withFilesInGit (whenAnnexed (start s))
				=<< workTreeItems (inprogressFiles o)

start :: S.Set Key -> FilePath -> Key -> CommandStart
start s _file k
	| S.member k s = start' k
	| otherwise = notInprogress

start' :: Key -> CommandStart
start' k = do
	tmpf <- fromRepo $ gitAnnexTmpObjectLocation k
	ifM (liftIO $ doesFileExist tmpf)
		( next $ next $ do
			liftIO $ putStrLn tmpf
			return True
		, notInprogress
		)

notInprogress :: CommandStart
notInprogress = next stop
