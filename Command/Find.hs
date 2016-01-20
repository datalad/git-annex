{- git-annex command
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Find where

import Data.Default
import qualified Data.Map as M

import Command
import Annex.Content
import Limit
import qualified Utility.Format
import Utility.DataUnits

cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $ mkCommand $
	command "find" SectionQuery "lists available files"
		paramPaths (seek <$$> optParser)

mkCommand :: Command -> Command
mkCommand = noCommit . noMessages . withGlobalOptions [jsonOption]

data FindOptions = FindOptions
	{ findThese :: CmdParams
	, formatOption :: Maybe Utility.Format.Format
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser FindOptions
optParser desc = FindOptions
	<$> cmdParams desc
	<*> optional parseFormatOption
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
	NoBatch -> withFilesInGit go (findThese o)
	Batch -> batchFiles go
  where
	go = whenAnnexed $ start o

-- only files inAnnex are shown, unless the user has requested
-- others via a limit
start :: FindOptions -> FilePath -> Key -> CommandStart
start o file key = ifM (limited <||> inAnnex key)
	( do
		showFormatted (formatOption o) file $ ("file", file) : keyVars key
		next $ next $ return True
	, stop
	)

showFormatted :: Maybe Utility.Format.Format -> String -> [(String, String)] -> Annex ()
showFormatted format unformatted vars =
	unlessM (showFullJSON vars) $
		case format of
			Nothing -> liftIO $ putStrLn unformatted
			Just formatter -> liftIO $ putStr $
				Utility.Format.format formatter $
					M.fromList vars

keyVars :: Key -> [(String, String)]
keyVars key =
	[ ("key", key2file key)
	, ("backend", keyBackendName key)
	, ("bytesize", size show)
	, ("humansize", size $ roughSize storageUnits True)
	, ("keyname", keyName key)
	, ("hashdirlower", hashDirLower def key)
	, ("hashdirmixed", hashDirMixed def key)
	, ("mtime", whenavail show $ keyMtime key)
	]
  where
	size c = whenavail c $ keySize key
	whenavail = maybe "unknown"
