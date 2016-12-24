{- git-annex command
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Wanted where

import Command
import qualified Annex
import qualified Remote
import Logs.PreferredContent
import Types.Messages
import Types.StandardGroups

import qualified Data.Map as M

cmd :: Command
cmd = cmd' "wanted" "get or set preferred content expression" 
	preferredContentMapRaw
	preferredContentSet

cmd'
	:: String
	-> String
	-> Annex (M.Map UUID PreferredContentExpression)
	-> (UUID -> PreferredContentExpression -> Annex ())
	-> Command
cmd' name desc getter setter = command name SectionSetup desc pdesc (withParams seek)
  where
	pdesc = paramPair paramRemote (paramOptional paramExpression)

	seek = withWords start

	start (rname:[]) = go rname (performGet getter)
	start (rname:expr:[]) = go rname $ \uuid -> do
		showStart name rname
		performSet setter expr uuid
	start _ = giveup "Specify a repository."
		
	go rname a = do
		u <- Remote.nameToUUID rname
		next $ a u

performGet :: Ord a => Annex (M.Map a PreferredContentExpression) -> a -> CommandPerform
performGet getter a = do
	Annex.setOutput QuietOutput
	m <- getter
	liftIO $ putStrLn $ fromMaybe "" $ M.lookup a m
	next $ return True

performSet :: (a -> PreferredContentExpression -> Annex ()) -> String -> a -> CommandPerform
performSet setter expr a = case checkPreferredContentExpression expr of
	Just e -> giveup $ "Parse error: " ++ e
	Nothing -> do
		setter a expr
		next $ return True
