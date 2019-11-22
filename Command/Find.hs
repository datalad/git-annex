{- git-annex command
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Find where

import Data.Default
import qualified Data.Map as M

import Command
import Annex.Content
import Limit
import Types.Key
import Git.FilePath
import qualified Utility.Format
import Utility.DataUnits

cmd :: Command
cmd = notBareRepo $ withGlobalOptions [annexedMatchingOptions] $ mkCommand $
	command "find" SectionQuery "lists available files"
		paramPaths (seek <$$> optParser)

mkCommand :: Command -> Command
mkCommand = noCommit . noMessages . withGlobalOptions [jsonOptions]

data FindOptions = FindOptions
	{ findThese :: CmdParams
	, formatOption :: Maybe Utility.Format.Format
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser FindOptions
optParser desc = FindOptions
	<$> cmdParams desc
	<*> optional parseFormatOption
	<*> optional parseBranchKeysOption
	<*> parseBatchOption

parseFormatOption :: Parser Utility.Format.Format
parseFormatOption = 
	option (Utility.Format.gen <$> str)
		( long "format" <> metavar paramFormat
		<> help "control format of output"
		)
	<|> flag' (Utility.Format.gen "${file}\0")
		( long "print0"
		<> help "output filenames terminated with nulls"
		)

seek :: FindOptions -> CommandSeek
seek o = case batchOption o of
	NoBatch -> withKeyOptions (keyOptions o) False
		(commandAction . startKeys o)
		(withFilesInGit (commandAction . go))
		=<< workTreeItems (findThese o)
	Batch fmt -> batchFilesMatching fmt go
  where
	go = whenAnnexed $ start o

-- only files inAnnex are shown, unless the user has requested
-- others via a limit
start :: FindOptions -> FilePath -> Key -> CommandStart
start o file key =
	stopUnless (limited <||> inAnnex key) $
		startingCustomOutput key $ do
			showFormatted (formatOption o) file $ ("file", file) : keyVars key
			next $ return True

startKeys :: FindOptions -> (Key, ActionItem) -> CommandStart
startKeys o (key, ActionItemBranchFilePath (BranchFilePath _ topf) _) = 
	start o (getTopFilePath topf) key
startKeys _ _ = stop

showFormatted :: Maybe Utility.Format.Format -> String -> [(String, String)] -> Annex ()
showFormatted format unformatted vars =
	unlessM (showFullJSON $ JSONChunk vars) $
		case format of
			Nothing -> liftIO $ putStrLn unformatted
			Just formatter -> liftIO $ putStr $
				Utility.Format.format formatter $
					M.fromList vars

keyVars :: Key -> [(String, String)]
keyVars key =
	[ ("key", serializeKey key)
	, ("backend", decodeBS $ formatKeyVariety $ fromKey keyVariety key)
	, ("bytesize", size show)
	, ("humansize", size $ roughSize storageUnits True)
	, ("keyname", decodeBS $ fromKey keyName key)
	, ("hashdirlower", hashDirLower def key)
	, ("hashdirmixed", hashDirMixed def key)
	, ("mtime", whenavail show $ fromKey keyMtime key)
	]
  where
	size c = whenavail c $ fromKey keySize key
	whenavail = maybe "unknown"
