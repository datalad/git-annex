{- git-annex command
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Group where

import Command
import qualified Remote
import Types.Group
import Logs.Group
import Logs.UUID
import Logs.Trust
import Utility.SafeOutput

import qualified Data.Set as S
import qualified Data.Map as M

cmd :: Command
cmd = noMessages $ command "group" SectionSetup "add a repository to a group"
	(paramPair paramRepository paramDesc) (seek <$$> optParser)

data GroupOptions = GroupOptions
	{ cmdparams :: CmdParams
	, listOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser GroupOptions
optParser desc = GroupOptions
	<$> cmdParamsWithCompleter desc completeRemotes
	<*> switch
		( long "list"
		<> help "list all currently defined groups"
		)

seek :: GroupOptions -> CommandSeek
seek o
	| listOption o = if null (cmdparams o)
		then commandAction startList
		else giveup "Cannot combine --list with other options"
	| otherwise = commandAction $ start (cmdparams o)

start :: [String] -> CommandStart
start ps@(name:g:[]) = do
	u <- Remote.nameToUUID name
	startingUsualMessages "group" ai si $
		setGroup u (toGroup g)
  where
	ai = ActionItemOther (Just (UnquotedString name))
	si = SeekInput ps
start (name:[]) = do
	u <- Remote.nameToUUID name
	startingCustomOutput (ActionItemOther Nothing) $ do
		liftIO . listGroups =<< lookupGroups u
		next $ return True
start _ = giveup "Specify a repository and a group."

startList :: CommandStart
startList = startingCustomOutput (ActionItemOther Nothing) $ do
	us <- trustExclude DeadTrusted =<< M.keys <$> uuidDescMap
	gs <- foldl' S.union mempty <$> mapM lookupGroups us
	liftIO $ listGroups gs
	next $ return True

listGroups :: S.Set Group -> IO ()
listGroups = liftIO . putStrLn . safeOutput . unwords . map fmt . S.toList
  where
	fmt (Group g) = decodeBS g

setGroup :: UUID -> Group -> CommandPerform
setGroup uuid g = do
	groupChange uuid (S.insert g) 
	next $ return True
