{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.List where

import qualified Data.Set as S

import Common.Annex
import Command
import Remote
import Logs.Trust
import Annex.UUID

def :: [Command]
def = [noCommit $ command "list" paramPaths seek
	SectionQuery "show which remotes contain files"]

seek :: [CommandSeek]
seek = 
	[ withValue getList $ \l -> withNothing $ startHeader l
	, withValue getList $ \l -> withFilesInGit $ whenAnnexed $ start l
	]

getList :: Annex [(UUID, RemoteName, TrustLevel)]
getList = do
	rs <- remoteList
	ts <- mapM (lookupTrust . uuid) rs
	hereu <- getUUID
	heretrust <- lookupTrust hereu
	return $ (hereu, "here", heretrust) : zip3 (map uuid rs) (map name rs) ts

startHeader :: [(UUID, RemoteName, TrustLevel)] -> CommandStart
startHeader l = do
	liftIO $ putStrLn $ header $ map (\(_, n, t) -> (n, t)) l
	stop

start :: [(UUID, RemoteName, TrustLevel)] -> FilePath -> (Key, Backend) -> CommandStart
start l file (key, _) = do
	ls <- S.fromList <$> keyLocations key
	liftIO $ putStrLn $ format (map (\(u, _, t) -> (t, S.member u ls)) l) file
	stop

type RemoteName = String
type Present = Bool

header :: [(RemoteName, TrustLevel)] -> String
header remotes = (unlines $ zipWith formatheader [0..] remotes) ++ (pipes (length remotes))
  where
    formatheader n (remotename, trustlevel) = (pipes n) ++ remotename ++ (trust trustlevel)
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
