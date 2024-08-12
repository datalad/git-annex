{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.MaxSize where

import Command
import qualified Remote
import Logs.MaxSize
import Utility.SafeOutput
import Utility.DataUnits

import qualified Data.Map as M

cmd :: Command
cmd = noMessages $ command "maxsize" SectionSetup
	"configure maximum size of repositoriy"
	(paramPair paramRepository (paramOptional paramSize))
	(seek <$$> optParser)

data MaxSizeOptions = MaxSizeOptions
	{ cmdparams :: CmdParams
	, bytesOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser MaxSizeOptions
optParser desc = MaxSizeOptions
	<$> cmdParams desc
	<*> switch
		( long "bytes"
		<> help "display sizes in bytes"
		)

seek :: MaxSizeOptions -> CommandSeek
seek o = case cmdparams o of
	(rname:[]) -> commandAction $ do
		u <- Remote.nameToUUID rname
		startingCustomOutput (ActionItemOther Nothing) $ do
			v <- M.lookup u <$> getMaxSizes
			liftIO $ putStrLn $ safeOutput $ case v of
				Just (MaxSize n) -> 
					if bytesOption o
						then show n
						else preciseSize storageUnits False n
				Nothing -> ""
			next $ return True
	(rname:sz:[]) -> commandAction $ do
		u <- Remote.nameToUUID rname
		let si = SeekInput (cmdparams o)
		let ai = ActionItemOther (Just (UnquotedString rname))
		startingUsualMessages "maxsize" ai si $
			case readSize dataUnits sz of
				Nothing -> giveup "Unable to parse size."
				Just n -> do
					recordMaxSize u (MaxSize n)
					next $ return True
	_ -> giveup "Specify a repository."
