{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Schedule where

import Command
import qualified Remote
import Logs.Schedule
import Types.ScheduledActivity

import qualified Data.Set as S

cmd :: Command
cmd = noMessages $ command "schedule" SectionSetup "get or set scheduled jobs"
	(paramPair paramRemote (paramOptional paramExpression))
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start = parse
  where
	parse (name:[]) = go name performGet
	parse (name:expr:[]) = go name $ \uuid -> do
		allowMessages
		showStart "schedule" name
		performSet expr uuid
	parse _ = giveup "Specify a repository."

	go name a = do
		u <- Remote.nameToUUID name
		next $ a u

performGet :: UUID -> CommandPerform
performGet uuid = do
	s <- scheduleGet uuid
	liftIO $ putStrLn $ intercalate "; " $ 
		map fromScheduledActivity $ S.toList s
	next $ return True

performSet :: String -> UUID -> CommandPerform
performSet expr uuid = case parseScheduledActivities expr of
	Left e -> giveup $ "Parse error: " ++ e
	Right l -> do
		scheduleSet uuid l
		next $ return True
