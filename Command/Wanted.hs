{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Wanted where

import Common.Annex
import qualified Annex
import Command
import qualified Remote
import Logs.PreferredContent
import Types.Messages

import qualified Data.Map as M

def :: [Command]
def = [command "wanted" (paramPair paramRemote (paramOptional paramExpression)) seek
	SectionSetup "get or set preferred content expression"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start = parse
  where
  	parse (name:[]) = go name performGet
	parse (name:expr:[]) = go name $ \uuid -> do
		showStart "wanted" name
		performSet expr uuid
	parse _ = error "Specify a repository."

	go name a = do
		u <- Remote.nameToUUID name
		next $ a u

performGet :: UUID -> CommandPerform
performGet uuid = do
	Annex.setOutput QuietOutput
	m <- preferredContentMapRaw
	liftIO $ putStrLn $ fromMaybe "" $ M.lookup uuid m
	next $ return True

performSet :: String -> UUID -> CommandPerform
performSet expr uuid = case checkPreferredContentExpression expr of
	Just e -> error $ "Parse error: " ++ e
	Nothing -> do
		preferredContentSet uuid expr
		next $ return True
