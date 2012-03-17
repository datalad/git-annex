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
def = [withOptions [formatOption, print0Option] $
	command "find" paramPaths seek "lists available files"]

formatOption :: Option
formatOption = Option.field [] "format" paramFormat "control format of output"

print0Option :: Option
print0Option = Option.Option [] ["print0"] (Option.NoArg set)
	"terminate output with null"
	where
		set = Annex.setField (Option.name formatOption) "${file}\0"

seek :: [CommandSeek]
seek = [withField formatOption formatconverter $ \f ->
		withFilesInGit $ whenAnnexed $ start f]
	where
		formatconverter = return . fmap Utility.Format.gen

start :: Maybe Utility.Format.Format -> FilePath -> (Key, Backend) -> CommandStart
start format file (key, _) = do
	-- only files inAnnex are shown, unless the user has requested
	-- others via a limit
	whenM (limited <||> inAnnex key) $
		unlessM (showFullJSON vars) $
			case format of
				Nothing -> liftIO $ putStrLn file
				Just formatter -> liftIO $ putStr $
					Utility.Format.format formatter $
						M.fromList vars
	stop
	where
		vars =
			[ ("file", file)
			, ("key", show key)
			, ("backend", keyBackendName key)
			, ("bytesize", size show)
			, ("humansize", size $ roughSize storageUnits True)
			]
		size c = maybe "unknown" c $ keySize key
