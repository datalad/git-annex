{- git-remote-annex program
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.GitRemoteAnnex where

import Annex.Common
import qualified Annex
import qualified Git.CurrentRepo
import Annex.UUID

run :: [String] -> IO ()
run (_remotename:url:[]) = do
	state <- Annex.new =<< Git.CurrentRepo.get
	Annex.eval state (run' url)
run (_remotename:[]) = giveup "remote url not configured"
run _ = giveup "expected remote name and url parameters"

run' :: String -> Annex ()
run' url = go =<< lines <$> liftIO getContents
  where
	go (l:ls) =
		let (c, v) = splitLine l
		in case c of
			"capabilities" -> capabilities >> go ls	
			"list" -> case v of
				"" -> list False >> go ls
				"for-push" -> list True >> go ls
				_ -> protocolError l
			"fetch" -> fetch (l:ls) >>= go
			"push" -> push (l:ls) >>= go
			_ -> protocolError l
	go [] = return ()

protocolError :: String -> a
protocolError l = giveup $ "gitremote-helpers protocol error at " ++ show l

capabilities :: Annex ()
capabilities = do
	liftIO $ putStrLn "fetch"
	liftIO $ putStrLn "push"
	liftIO $ putStrLn ""
	liftIO $ hFlush stdout

list :: Bool -> Annex ()
list forpush = error "TODO list" 

-- Any number of fetch commands can be sent by git, asking for specific
-- things. We fetch everything new at once, so find the end of the fetch
-- commands (which is supposed to be a blank line) before fetching. 
fetch :: [String] -> Annex [String]
fetch (l:ls) = case splitLine l of
	("fetch", _) -> fetch ls
	("", _) -> do
		fetch'
		return ls
	_ -> do
		fetch'
		return (l:ls)
fetch [] = do
	fetch'
	return []

fetch' :: Annex ()
fetch' = error "TODO fetch"

push :: [String] -> Annex [String]
push ls = do
	let (refspecs, ls') = collectRefSpecs ls
	error "TODO push refspecs"
	return ls'

data RefSpec = RefSpec
	{ forcedPush :: Bool
	, srcRef :: Maybe String -- empty when deleting a ref
	, dstRef :: String
	}
	deriving (Show)

-- Any number of push commands can be sent by git, specifying the refspecs
-- to push. They should be followed by a blank line.
collectRefSpecs :: [String] -> ([RefSpec], [String])
collectRefSpecs = go []
  where
	go c (l:ls) = case splitLine l of
		("push", refspec) -> go (parseRefSpec refspec:c) ls
		("", _) -> (c, ls)
		_ -> (c, (l:ls))
	go c [] = (c, [])

parseRefSpec :: String -> RefSpec
parseRefSpec ('+':s) = (parseRefSpec s) { forcedPush = True }
parseRefSpec s = 
	let (src, cdst) = break (== ':') s
	    dst = if null cdst then cdst else drop 1 cdst
	in RefSpec
		{ forcedPush = False
		, srcRef = if null src then Nothing else Just src
		, dstRef = dst
		}

-- "foo bar" to ("foo", "bar")
-- "foo" to ("foo", "")
splitLine :: String -> (String, String)
splitLine l = 
	let (c, sv) = break (== ' ') l
	    v = if null sv then sv else drop 1 sv
	in (c, v)
