{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
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

import Command
import Remote
import Logs.Trust
import Logs.UUID
import Annex.UUID
import Git.Types (RemoteName)

cmd :: Command
cmd = noCommit $ withGlobalOptions annexedMatchingOptions $
	command "list" SectionQuery 
		"show which remotes contain files"
		paramPaths (seek <$$> optParser)

data ListOptions = ListOptions
	{ listThese :: CmdParams
	, allRepos :: Bool
	}

optParser :: CmdParamsDesc -> Parser ListOptions
optParser desc = ListOptions
	<$> cmdParams desc
	<*> switch
		( long "allrepos"
		<> help "show all repositories, not only remotes"
		)

seek :: ListOptions -> CommandSeek
seek o = do
	list <- getList o
	printHeader list
	withFilesInGit (whenAnnexed $ start list) (listThese o)

getList :: ListOptions -> Annex [(UUID, RemoteName, TrustLevel)]
getList o
	| allRepos o = nubBy ((==) `on` fst3) <$> ((++) <$> getRemotes <*> getAllUUIDs)
	| otherwise = getRemotes
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
printHeader l = liftIO $ putStrLn $ lheader $ map (\(_, n, t) -> (n, t)) l

start :: [(UUID, RemoteName, TrustLevel)] -> FilePath -> Key -> CommandStart
start l file key = do
	ls <- S.fromList <$> keyLocations key
	liftIO $ putStrLn $ format (map (\(u, _, t) -> (t, S.member u ls)) l) file
	stop

type Present = Bool

lheader :: [(RemoteName, TrustLevel)] -> String
lheader remotes = unlines (zipWith formatheader [0..] remotes) ++ pipes (length remotes)
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
