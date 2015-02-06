{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.GroupWanted where

import Common.Annex
import qualified Annex
import Command
import Logs.PreferredContent
import Types.Messages
import Types.Group

import qualified Data.Map as M

cmd :: [Command]
cmd = [command "groupwanted" (paramPair paramGroup (paramOptional paramExpression)) seek
	SectionSetup "get or set groupwanted expression"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (g:[]) = next $ performGet g
start (g:expr:[]) = do
	showStart "groupwanted" g
	next $ performSet g expr
start _ = error "Specify a group."

performGet :: Group -> CommandPerform
performGet g = do
	Annex.setOutput QuietOutput
	m <- groupPreferredContentMapRaw
	liftIO $ putStrLn $ fromMaybe "" $ M.lookup g m
	next $ return True

performSet :: Group -> String -> CommandPerform
performSet g expr = case checkPreferredContentExpression expr of
	Just e -> error $ "Parse error: " ++ e
	Nothing -> do
		groupPreferredContentSet g expr
		next $ return True
