{- git-annex command
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.WhereUsed where

import Command
import Git.FilePath
import Annex.CatFile
import Database.Keys

cmd :: Command
cmd = noCommit $ withGlobalOptions [annexedMatchingOptions] $
	command "whereused" SectionQuery
		"lists repositories that have file content"
		paramNothing (seek <$$> optParser)

data WhereUsedOptions = WhereUsedOptions
	{ keyOptions :: KeyOptions
	, historicalOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser WhereUsedOptions
optParser _desc = WhereUsedOptions
	<$> (parseUnusedKeysOption <|> parseSpecificKeyOption)
	<*> switch
		( long "historical"
		<> help "find historical uses"
		)

seek :: WhereUsedOptions -> CommandSeek
seek o = withKeyOptions (Just (keyOptions o)) False dummyfileseeker
	(commandAction . start o) dummyfilecommandseek (WorkTreeItems [])
  where
	dummyfileseeker = AnnexedFileSeeker
		{ startAction = \_ _ _ -> return Nothing
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}
	dummyfilecommandseek = const noop

start :: WhereUsedOptions -> (SeekInput, Key, ActionItem) -> CommandStart
start o (_, key, _) = startingCustomOutput key $ do
	fs <- filterM stillassociated 
		=<< mapM (fromRepo . fromTopFilePath)
		=<< getAssociatedFiles key
	liftIO $ forM_ fs $ display key . fromRawFilePath

	when (historicalOption o && null fs) $
		findHistorical key

	next $ return True
  where
	-- Some associated files that are in the keys database may no
	-- longer correspond to files in the repository.
	stillassociated f = catKeyFile f >>= \case
		Just k | k == key -> return True
		_ -> return False

display :: Key -> FilePath -> IO ()
display key f = putStrLn (serializeKey key ++ " " ++ f)

findHistorical :: Key -> Annex ()
findHistorical key = do
	error "TODO"
