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
import qualified Option

def :: [Command]
def = [noCommit $ noMessages $ withOptions [formatOption, print0Option] $
	command "find" paramPaths seek SectionQuery "lists available files"]

formatOption :: Option
formatOption = Option.field [] "format" paramFormat "control format of output"

withFormat :: (Maybe Utility.Format.Format -> CommandSeek) -> CommandSeek
withFormat = withField formatOption $ return . fmap Utility.Format.gen

print0Option :: Option
print0Option = Option.Option [] ["print0"] (Option.NoArg set)
	"terminate output with null"
  where
	set = Annex.setField (Option.name formatOption) "${file}\0"

seek :: [CommandSeek]
seek = [withFormat $ \f -> withFilesInGit $ whenAnnexed $ start f]

start :: Maybe Utility.Format.Format -> FilePath -> (Key, Backend) -> CommandStart
start format file (key, _) = do
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
