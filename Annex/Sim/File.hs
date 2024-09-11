{- sim files
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Sim.File where

import Annex.Sim
import Annex.Common
import Utility.DataUnits

import Data.Char
import Text.Read

parseSimFile :: String -> Either String [SimCommand]
parseSimFile = go [] . lines
  where
	go c [] = Right c
	go c (l:ls) = case parseSimFileLine l of
		Right cs -> go (c ++ cs) ls
		Left err -> Left err

parseSimFileLine :: String -> Either String [SimCommand]
parseSimFileLine s
	| "#" `isPrefixOf` s = Right [CommandComment s]
	| all isSpace s = Right [CommandBlank]
	| otherwise = case words s of
		("init":name:[]) ->
			Right [CommandInit (RepoName name)]
		("initremote":name:[]) ->
			Right [CommandInitRemote (RepoName name)]
		("use":name:rest) ->
			Right [CommandUse (RepoName name) (unwords rest)]
		("connect":rest) ->
			parseConnect CommandConnect rest
		("disconnect":rest) ->
			parseConnect CommandDisconnect rest
		("addtree":name:rest) ->
			Right [CommandAddTree(RepoName name) (unwords rest)]
		("add":filename:size:repos) ->
			case readSize dataUnits size of
				Just sz -> Right [CommandAdd (toRawFilePath filename) sz (map RepoName repos)]
				Nothing -> Left $ "Unable to parse file size \"" ++ size ++ "\""
		("step":n:[]) ->
			case readMaybe n of
				Just n' -> Right [CommandStep n']
				Nothing -> Left $ "Unable to parse step value \"" ++ n ++ "\""
		("action":repo:"pull":remote:[]) ->
			Right [CommandAction (RepoName repo) (ActionPull (RemoteName remote))]
		("action":repo:"push":remote:[]) ->
			Right [CommandAction (RepoName repo) (ActionPush (RemoteName remote))]
		("action":repo:"getwanted":remote:[]) ->
			Right [CommandAction (RepoName repo) (ActionGetWanted (RemoteName remote))]
		("action":repo:"dropunwanted":[]) ->
			Right [CommandAction (RepoName repo) (ActionDropUnwanted Nothing)]
		("action":repo:"dropunwanted":remote:[]) ->
			Right [CommandAction (RepoName repo) (ActionDropUnwanted (Just (RemoteName remote)))]
		("action":repo:"gitpush":remote:[]) ->
			Right [CommandAction (RepoName repo) (ActionGitPush (RemoteName remote))]
		("action":repo:"gitpull":remote:[]) ->
			Right [CommandAction (RepoName repo) (ActionGitPull (RemoteName remote))]
		("seed":n:[]) ->
			case readMaybe n of
				Just n' -> Right [CommandSeed n']
				Nothing -> Left $ "Unable to parse seed value \"" ++ n ++ "\""
		("present":repo:file:[]) ->
			Right [CommandPresent (RepoName repo) (toRawFilePath file)]
		("notpresent":repo:file:[]) ->
			Right [CommandNotPresent (RepoName repo) (toRawFilePath file)]
		-- TODO rest
		_ -> Left $ "Unable to parse sim file line: \"" ++ s ++ "\""

parseConnect :: (RepoName -> RemoteName -> SimCommand) -> [String] -> Either String [SimCommand]
parseConnect mk = go []
  where
	go c [] = Right c
	go c (r1:"->":r2:rest) = 
		go (mk (RepoName r1) (RemoteName r2):c)
			(chain r2 rest)
	go c (r1:"<-":r2:rest) = 
		go (mk (RepoName r2) (RemoteName r1):c)
			(chain r2 rest)
	go c (r1:"<->":r2:rest) = 
		go (mk (RepoName r2) (RemoteName r1)
			: mk (RepoName r1) (RemoteName r2)
			: c
			)
			(chain r2 rest)
	go _ rest = Left $ "Bad connect syntax near \"" ++ unwords rest ++ "\""

	chain v rest = if null rest then rest else v:rest

