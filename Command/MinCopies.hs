{- git-annex command
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.MinCopies where

import Command
import Annex.NumCopies
import qualified Command.NumCopies

cmd :: Command
cmd = noMessages $ command "mincopies" SectionSetup 
	"configure minimum number of copies"
	paramNumber (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . Command.NumCopies.start' "mincopies" startGet startSet)

start :: [String] -> CommandStart
start = Command.NumCopies.start' "mincopies" startGet startSet

startGet :: CommandStart
startGet = startingCustomOutput (ActionItemOther Nothing) $ next $ do
	v <- getGlobalMinCopies
	case v of
		Just n -> liftIO $ putStrLn $ show $ fromMinCopies n
		Nothing -> liftIO $ putStrLn "global mincopies is not set"
	return True

startSet :: Int -> CommandStart
startSet n = startingUsualMessages "mincopies" ai si $ do
	setGlobalMinCopies $ MinCopies n
	next $ return True
  where
	ai = ActionItemOther (Just $ show n)
	si = SeekInput [show n]
