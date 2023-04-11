{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Schedule where

import Command
import qualified Remote
import Logs.Schedule
import Types.ScheduledActivity
import Utility.SafeOutput

import qualified Data.Set as S

cmd :: Command
cmd = noMessages $ command "schedule" SectionSetup "get or set scheduled jobs"
	(paramPair paramRemote (paramOptional paramExpression))
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start = parse
  where
	parse (name:[]) = do
		u <- Remote.nameToUUID name
		startingCustomOutput (ActionItemOther Nothing) $
			performGet u
	parse ps@(name:expr:[]) = do
		u <- Remote.nameToUUID name
		let ai = ActionItemOther (Just (UnquotedString name))
		let si = SeekInput ps
		startingUsualMessages "schedule" ai si $
			performSet expr u
	parse _ = giveup "Specify a repository."

performGet :: UUID -> CommandPerform
performGet uuid = do
	s <- scheduleGet uuid
	liftIO $ putStrLn $ safeOutput $ intercalate "; " $ 
		map fromScheduledActivity $ S.toList s
	next $ return True

performSet :: String -> UUID -> CommandPerform
performSet expr uuid = case parseScheduledActivities expr of
	Left e -> giveup $ "Parse error: " ++ e
	Right l -> do
		scheduleSet uuid l
		next $ return True
