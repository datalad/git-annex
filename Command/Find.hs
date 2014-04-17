{- git-annex command
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Find where

import qualified Data.Map as M

import Common.Annex
import Command
import Annex.Content
import Limit
import qualified Annex
import qualified Utility.Format
import Utility.DataUnits
import Types.Key

def :: [Command]
def = [mkCommand $ command "find" paramPaths seek SectionQuery "lists available files"]

mkCommand :: Command -> Command
mkCommand = noCommit . noMessages . withOptions [formatOption, print0Option, jsonOption]

formatOption :: Option
formatOption = fieldOption [] "format" paramFormat "control format of output"

getFormat :: Annex (Maybe Utility.Format.Format)
getFormat = getOptionField formatOption $ return . fmap Utility.Format.gen

print0Option :: Option
print0Option = Option [] ["print0"] (NoArg set)
	"terminate output with null"
  where
	set = Annex.setField (optionName formatOption) "${file}\0"

seek :: CommandSeek
seek ps = do
	format <- getFormat
	withFilesInGit (whenAnnexed $ start format) ps

start :: Maybe Utility.Format.Format -> FilePath -> Key -> CommandStart
start format file key = do
	-- only files inAnnex are shown, unless the user has requested
	-- others via a limit
	whenM (limited <||> inAnnex key) $
		showFormatted format file $ ("file", file) : keyVars key
	stop

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
	, ("hashdirlower", hashDirLower key)
	, ("hashdirmixed", hashDirMixed key)
	, ("mtime", whenavail show $ keyMtime key)
	]
  where
	size c = whenavail c $ keySize key
	whenavail = maybe "unknown"
