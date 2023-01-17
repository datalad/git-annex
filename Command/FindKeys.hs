{- git-annex command
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.FindKeys where

import Command
import qualified Utility.Format
import qualified Command.Find

cmd :: Command
cmd = withAnnexOptions [keyMatchingOptions] $ Command.Find.mkCommand $
	command "findkeys" SectionQuery "lists available keys"
		paramNothing (seek <$$> optParser)

data FindKeysOptions = FindKeysOptions
	{ formatOption :: Maybe Utility.Format.Format
	}

optParser :: CmdParamsDesc -> Parser FindKeysOptions
optParser _ = FindKeysOptions
	<$> optional (Command.Find.parseFormatOption' "${key}\0")

seek :: FindKeysOptions -> CommandSeek
seek o = do
	seeker <- Command.Find.contentPresentUnlessLimited $ AnnexedFileSeeker
		{ checkContentPresent = Nothing
		, usesLocationLog = False
		-- startAction is not actually used since this
		-- is not used to seek files
		, startAction = \_ _ key -> start' o key
		}
	withKeyOptions (Just WantAllKeys) False seeker
		(commandAction . start o)
		(const noop) (WorkTreeItems [])

start :: FindKeysOptions -> (SeekInput, Key, ActionItem) -> CommandStart
start o (_si, key, _ai) = start' o key

start' :: FindKeysOptions -> Key -> CommandStart
start' o key = startingCustomOutput key $ do
	Command.Find.showFormatted (formatOption o) (serializeKey' key)
		(Command.Find.formatVars key (AssociatedFile Nothing))
	next $ return True
