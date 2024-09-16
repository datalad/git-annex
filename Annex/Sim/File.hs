{- sim files
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Sim.File where

import Annex.Sim
import Annex.Common hiding (group)
import Utility.DataUnits
import Types.TrustLevel
import Types.Group
import Git.Config (isTrueFalse)

import Data.Char
import Text.Read

parseSimFile :: String -> Either String [SimCommand]
parseSimFile = go [] . lines
  where
	go cs [] = Right (reverse cs)
	go cs (l:ls) = case parseSimFileLine l of
		Right command -> go (command:cs) ls
		Left err -> Left err

parseSimFileLine :: String -> Either String SimCommand
parseSimFileLine s
	| "#" `isPrefixOf` s = Right (CommandComment s)
	| all isSpace s = Right (CommandBlank)
	| otherwise = parseSimCommand (words s)

generateSimFile :: [SimCommand] -> String
generateSimFile = unlines . map unwords . go
  where
	go [] = []
	go (CommandInit (RepoName name) : rest) = 
		["init", name] : go rest
	go (CommandInitRemote (RepoName name) : rest) =
		["initremote", name] : go rest
	go (CommandUse (RepoName name) what : rest) =
		["use", name, what] : go rest
	go (CommandConnect c : rest) =
		("connect":formatConnections c) : go rest
	go (CommandDisconnect c : rest) =
		("disconnect":formatConnections c) : go rest
	go (CommandAddTree (RepoName name) expr : rest) =
		["addtree", name, expr] : go rest
	go (CommandAdd f sz repos : rest) =
		(["add", fromRawFilePath f, showsize sz] ++ map fromRepoName repos) : go rest
	go (CommandStep n : rest) =
		["step", show n] : go rest
	go (CommandAction (RepoName repo) (ActionPull (RemoteName remote)) : rest) =
		["action", repo, "pull", remote] : go rest
	go (CommandAction (RepoName repo) (ActionPush (RemoteName remote)) : rest) =
		["action", repo, "push", remote] : go rest
	go (CommandAction (RepoName repo) (ActionSync (RemoteName remote)) : rest) =
		["action", repo, "sync", remote] : go rest
	go (CommandAction (RepoName repo) (ActionGetWanted (RemoteName remote)) : rest) =
		["action", repo, "getwanted", remote] : go rest
	go (CommandAction (RepoName repo) (ActionDropUnwanted (Just (RemoteName remote))) : rest) =
		["action", repo, "dropunwanted", remote] : go rest
	go (CommandAction (RepoName repo) (ActionDropUnwanted Nothing) : rest) =
		["action", repo, "dropunwanted"] : go rest
	go (CommandAction (RepoName repo) (ActionSendWanted (RemoteName remote)) : rest) =
		["action", repo, "sendwanted", remote] : go rest
	go (CommandAction (RepoName repo) (ActionGitPush (RemoteName remote)) : rest) =
		["action", repo, "gitpush", remote] : go rest
	go (CommandAction (RepoName repo) (ActionGitPull (RemoteName remote)) : rest) =
		["action", repo, "gitpull", remote] : go rest
	go (CommandSeed n : rest) =
		["seed", show n] : go rest
	go (CommandPresent (RepoName repo) f : rest) =
		["present", repo, fromRawFilePath f] : go rest
	go (CommandNotPresent (RepoName repo) f : rest) =
		["notpresent", repo, fromRawFilePath f] : go rest
	go (CommandNumCopies n : rest) = 
		["numcopies", show n] : go rest
	go (CommandMinCopies n : rest) =
		["mincopies", show n] : go rest
	go (CommandTrustLevel (RepoName repo) trustlevel : rest) =
		["trustlevel", repo, showTrustLevel trustlevel] : go rest
	go (CommandGroup (RepoName repo) group : rest) =
		["group", repo, fromGroup group] : go rest
	go (CommandUngroup (RepoName repo) group : rest) =
		["ungroup", repo, fromGroup group] : go rest
	go (CommandWanted (RepoName repo) expr : rest) =
		["wanted", repo, expr] : go rest
	go (CommandRequired (RepoName repo) expr : rest) =
		["required", repo, expr] : go rest
	go (CommandGroupWanted group expr : rest) =
		["groupwanted", fromGroup group, expr] : go rest
	go (CommandMaxSize (RepoName repo) maxsize : rest) =
		["maxsize", repo, showsize (fromMaxSize maxsize)] : go rest
	go (CommandRebalance b : rest) =
		["rebalance", if b then "on" else "off"] : go rest
	go (CommandComment s : rest) = 
		[s] : go rest
	go (CommandBlank : rest) = 
		[""] : go rest

	showsize = filter (not . isSpace) . preciseSize storageUnits True

parseSimCommand :: [String] -> Either String SimCommand
parseSimCommand ("init":name:[]) = 
	Right $ CommandInit (RepoName name)
parseSimCommand ("initremote":name:[]) =
	Right $ CommandInitRemote (RepoName name)
parseSimCommand ("use":name:rest) =
	Right $ CommandUse (RepoName name) (unwords rest)
parseSimCommand	("connect":rest) = 
	CommandConnect <$> parseConnections rest
parseSimCommand	("disconnect":rest) =
	CommandDisconnect <$> parseConnections rest
parseSimCommand ("addtree":name:rest) =
	Right $ CommandAddTree(RepoName name) (unwords rest)
parseSimCommand ("add":filename:size:repos) =
	case readSize dataUnits size of
		Just sz -> Right $ CommandAdd
			(toRawFilePath filename)
			sz
			(map RepoName repos)
		Nothing -> Left $ "Unable to parse file size \"" ++ size ++ "\""
parseSimCommand ("step":n:[]) =
	case readMaybe n of
			Just n' -> Right $ CommandStep n'
			Nothing -> Left $ "Unable to parse step value \"" ++ n ++ "\""
parseSimCommand ("action":repo:"pull":remote:[]) =
	Right $ CommandAction (RepoName repo)
		(ActionPull (RemoteName remote))
parseSimCommand ("action":repo:"push":remote:[]) =
	Right $ CommandAction (RepoName repo)
		(ActionPush (RemoteName remote))
parseSimCommand ("action":repo:"sync":remote:[]) =
	Right $ CommandAction (RepoName repo)
		(ActionSync (RemoteName remote))
parseSimCommand ("action":repo:"getwanted":remote:[]) =
	Right $ CommandAction (RepoName repo)
		(ActionGetWanted (RemoteName remote))
parseSimCommand	("action":repo:"dropunwanted":[]) =
	Right $ CommandAction (RepoName repo)
		(ActionDropUnwanted Nothing)
parseSimCommand ("action":repo:"dropunwanted":remote:[]) =
	Right $ CommandAction (RepoName repo)
		(ActionDropUnwanted (Just (RemoteName remote)))
parseSimCommand ("action":repo:"gitpush":remote:[]) =
	Right $ CommandAction (RepoName repo) 
		(ActionGitPush (RemoteName remote))
parseSimCommand ("action":repo:"gitpull":remote:[]) =
	Right $ CommandAction (RepoName repo)
		(ActionGitPull (RemoteName remote))
parseSimCommand	("seed":n:[]) =
	case readMaybe n of
		Just n' -> Right $ CommandSeed n'
		Nothing -> Left $ "Unable to parse seed value \"" ++ n ++ "\""
parseSimCommand ("present":repo:file:[]) =
	Right $ CommandPresent (RepoName repo) (toRawFilePath file)
parseSimCommand ("notpresent":repo:file:[]) =
	Right $ CommandNotPresent (RepoName repo) (toRawFilePath file)
parseSimCommand ("numcopies":n:[]) =
	case readMaybe n of
		Just n' -> Right $ CommandNumCopies n'
		Nothing -> Left $ "Unable to parse numcopies value \"" ++ n ++ "\""
parseSimCommand ("mincopies":n:[]) =
	case readMaybe n of
		Just n' -> Right $ CommandMinCopies n'
		Nothing -> Left $ "Unable to parse mincopies value \"" ++ n ++ "\""
parseSimCommand ("trustlevel":repo:s:[]) =
	case readTrustLevel s of
		Just trustlevel -> Right $ 
			CommandTrustLevel (RepoName repo) trustlevel
		Nothing -> Left $ "Unknown trust level \"" ++ s ++ "\"."
parseSimCommand ("group":repo:group:[]) =
	Right $ CommandGroup (RepoName repo) (toGroup group)
parseSimCommand ("ungroup":repo:group:[]) =
	Right $ CommandUngroup (RepoName repo) (toGroup group)
parseSimCommand ("wanted":repo:expr) =
	Right $ CommandWanted (RepoName repo) (unwords expr)
parseSimCommand ("required":repo:expr) =
	Right $ CommandRequired (RepoName repo) (unwords expr)
parseSimCommand ("groupwanted":group:expr) =
	Right $ CommandGroupWanted (toGroup group) (unwords expr)
parseSimCommand ("maxsize":repo:size:[]) =
	case readSize dataUnits size of
		Just sz -> Right $ CommandMaxSize (RepoName repo) (MaxSize sz)
		Nothing -> Left $ "Unable to parse maxsize \"" ++ size ++ "\""
parseSimCommand ("rebalance":onoff:[]) = case isTrueFalse onoff of
	Just b -> Right $ CommandRebalance b
	Nothing -> Left $ "Unable to parse rebalance value \"" ++ onoff ++ "\""
parseSimCommand ws = Left $ "Unable to parse sim command: \"" ++ unwords ws ++ "\""

parseConnections :: [String] -> Either String Connections
parseConnections = go . reverse
  where
	go (r2:"->":r1:rest) = 
		chain (RepoName r1 :-> RemoteName r2) rest
	go (r2:"<-":r1:rest) = 
		chain (RemoteName r1 :<- RepoName r2) rest
	go (r2:"<->":r1:rest) = 
		chain (RepoName r1 :<-> RepoName r2) rest
	go rest = bad rest

	chain c [] = Right c
	chain c ("->":r:rest) = chain (RepoName r :=> c) rest
	chain c ("<-":r:rest) = chain (RemoteName r :<= c) rest
	chain c ("<->":r:rest) = chain (RepoName r :<=> c) rest
	chain _ rest = bad rest

	bad rest = Left $ "Bad connect syntax near \"" ++ unwords rest ++ "\""

formatConnections :: Connections -> [String]
formatConnections (RepoName repo :-> RemoteName remote) =
	[repo, "->", remote]
formatConnections (RemoteName remote :<- RepoName repo) =
	[remote, "<-", repo]
formatConnections (RepoName repo1 :<-> RepoName repo2) =
	[repo1, "<->", repo2]
formatConnections (RepoName repo :=> c) =
	repo : "->" : formatConnections c
formatConnections (RemoteName remote :<= c) =
	remote : "<-" : formatConnections c
formatConnections (RepoName repo :<=> c) =
	repo : "<->" : formatConnections c

