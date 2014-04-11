{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 - Copyright 2013 Antoine Beaupré
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.List where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Function
import Data.Tuple.Utils
import Data.Ord

import Common.Annex
import Command
import Remote
import Logs.Trust
import Logs.UUID
import Annex.UUID
import qualified Annex
import Git.Types (RemoteName)

def :: [Command]
def = [noCommit $ withOptions [allrepos] $ command "list" paramPaths seek
	SectionQuery "show which remotes contain files"]

allrepos :: Option
allrepos = flagOption [] "allrepos" "show all repositories, not only remotes"

seek :: CommandSeek
seek ps = do
	list <- getList
	printHeader list
	withFilesInGit (whenAnnexed $ start list) ps

getList :: Annex [(UUID, RemoteName, TrustLevel)]
getList = ifM (Annex.getFlag $ optionName allrepos)
	( nubBy ((==) `on` fst3) <$> ((++) <$> getRemotes <*> getAllUUIDs)
	, getRemotes
	)
  where
	getRemotes = do
		rs <- remoteList
		ts <- mapM (lookupTrust . uuid) rs
		hereu <- getUUID
		heretrust <- lookupTrust hereu
		return $ (hereu, "here", heretrust) : zip3 (map uuid rs) (map name rs) ts
	getAllUUIDs = do
		rs <- M.toList <$> uuidMap
		rs3 <- forM rs $ \(u, n) -> (,,)
			<$> pure u
			<*> pure n
			<*> lookupTrust u
		return $ sortBy (comparing snd3) $
			filter (\t -> thd3 t /= DeadTrusted) rs3

printHeader :: [(UUID, RemoteName, TrustLevel)] -> Annex ()
printHeader l = liftIO $ putStrLn $ header $ map (\(_, n, t) -> (n, t)) l

start :: [(UUID, RemoteName, TrustLevel)] -> FilePath -> (Key, Backend) -> CommandStart
start l file (key, _) = do
	ls <- S.fromList <$> keyLocations key
	liftIO $ putStrLn $ format (map (\(u, _, t) -> (t, S.member u ls)) l) file
	stop

type Present = Bool

header :: [(RemoteName, TrustLevel)] -> String
header remotes = unlines (zipWith formatheader [0..] remotes) ++ pipes (length remotes)
  where
    formatheader n (remotename, trustlevel) = pipes n ++ remotename ++ trust trustlevel
    pipes = flip replicate '|'
    trust UnTrusted = " (untrusted)"
    trust _ = ""

format :: [(TrustLevel, Present)] -> FilePath -> String
format remotes file = thereMap ++ " " ++ file
  where 
    thereMap = concatMap there remotes
    there (UnTrusted, True) = "x"
    there (_, True) = "X"
    there (_, False) = "_"
