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
def = [withOptions [formatOption, print0Option] $
	command "find" paramPaths seek "lists available files"]

print0Option :: Option
print0Option = Option [] ["print0"] (NoArg $ Annex.setField "format" "${file}\0")
	"terminate output with null"

formatOption :: Option
formatOption = fieldOption [] "format" paramFormat "control format of output"

seek :: [CommandSeek]
seek = [withField "format" formatconverter $ \f ->
		withFilesInGit $ whenAnnexed $ start f]
	where
		formatconverter = maybe Nothing (Just . Utility.Format.gen)

start :: Maybe Utility.Format.Format -> FilePath -> (Key, Backend) -> CommandStart
start format file (key, _) = do
	-- only files inAnnex are shown, unless the user has requested
	-- others via a limit
	whenM (liftM2 (||) limited (inAnnex key)) $
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
