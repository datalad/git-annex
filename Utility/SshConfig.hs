{- ssh config file parsing and modification
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.SshConfig where

import Common
import Utility.UserInfo
import Utility.Tmp
import Utility.FileMode

import Data.Char
import Data.Ord
import Data.Either
import System.Directory

data SshConfig
	= GlobalConfig SshSetting
	| HostConfig Host [Either Comment SshSetting]
	| CommentLine Comment
	deriving (Show)

data Comment = Comment Indent String
	deriving (Show)

data SshSetting = SshSetting Indent Key Value
	deriving (Show)

type Indent = String
type Host = String
type Key = String
type Value = String

{- Parses ~/.ssh/config. Comments and indentation are preserved.
 -
 - Note that there is no parse failure. If a line cannot be parsed, it will
 - be taken to be a SshSetting that contains the whole line as the key,
 - and has no value. -}
parseSshConfig :: String -> [SshConfig]
parseSshConfig = go [] . lines
  where
	go c [] = reverse c
	go c (l:ls)
		| iscomment l = collect $ CommentLine $ mkcomment l
		| otherwise = case splitline l of
			(indent, k, v)
				| isHost k -> hoststanza v c [] ls
				| otherwise -> collect $ GlobalConfig $ SshSetting indent k v
	  where
		collect v = go (v:c) ls

	hoststanza host c hc [] = go (HostConfig host (reverse hc):c) []
	hoststanza host c hc (l:ls)
		| iscomment l = hoststanza host c ((Left $ mkcomment l):hc) ls
		| otherwise = case splitline l of
			(indent, k, v)
				| isHost k -> hoststanza v
					(HostConfig host (reverse hc):c) [] ls
				| otherwise -> hoststanza host c
					((Right $ SshSetting indent k v):hc) ls

	iscomment l = all isSpace l || "#" `isPrefixOf` (dropWhile isSpace l)

	splitline l = (indent, k, v)
	  where
		(indent, rest) = span isSpace l
		(k, v) = separate isSpace rest
	
	mkcomment l = Comment indent c
	  where
		(indent, c) = span isSpace l

	isHost v = map toLower v == "host"
	
genSshConfig :: [SshConfig] -> String
genSshConfig = unlines . concatMap gen
  where
	gen (CommentLine c) = [comment c]
	gen (GlobalConfig s) = [setting s]
	gen (HostConfig h cs) = ("Host " ++ h) : map (either comment setting) cs

	setting (SshSetting indent k v) = indent ++ k ++ " " ++ v
	comment (Comment indent c) = indent ++ c

findHostConfigKey :: SshConfig -> Key -> Maybe Value
findHostConfigKey (HostConfig _ cs) wantk = go (rights cs) (map toLower wantk)
  where
	go [] _ = Nothing
	go ((SshSetting _ k v):rest) wantk'
		| map toLower k == wantk' = Just v
		| otherwise = go rest wantk'
findHostConfigKey _ _ = Nothing

{- Adds a particular Key and Value to a HostConfig. -}
addToHostConfig :: SshConfig -> Key -> Value -> SshConfig
addToHostConfig (HostConfig host cs) k v = 
	HostConfig host $ Right (SshSetting indent k v) : cs
  where
	{- The indent is taken from any existing SshSetting
	 - in the HostConfig (largest indent wins). -}
	indent = fromMaybe "\t" $ headMaybe $ reverse $
		sortBy (comparing length) $ map getindent cs
	getindent (Right (SshSetting i _ _)) = i
	getindent (Left (Comment i _)) = i
addToHostConfig other _ _ = other

modifyUserSshConfig :: ([SshConfig] -> [SshConfig]) -> IO ()
modifyUserSshConfig modifier = changeUserSshConfig $ 
	genSshConfig . modifier . parseSshConfig

changeUserSshConfig :: (String -> String) -> IO ()
changeUserSshConfig modifier = do
	sshdir <- sshDir
	let configfile = sshdir </> "config"
	whenM (doesFileExist configfile) $ do
		c <- readFileStrict configfile
		let c' = modifier c
		when (c /= c') $ do
			-- If it's a symlink, replace the file it
			-- points to.
			f <- catchDefaultIO configfile (canonicalizePath configfile)
			viaTmp writeSshConfig f c'

writeSshConfig :: FilePath -> String -> IO ()
writeSshConfig f s = do
	writeFile f s
	setSshConfigMode f

{- Ensure that the ssh config file lacks any group or other write bits, 
 - since ssh is paranoid about not working if other users can write
 - to one of its config files (.ssh/config and .ssh/authorized_keys).
 -
 - If the chmod fails, ignore the failure, as it might be a filesystem like
 - Android's that does not support file modes.
 -}
setSshConfigMode :: FilePath -> IO ()
setSshConfigMode f = void $ tryIO $ modifyFileMode f $
	removeModes [groupWriteMode, otherWriteMode]

sshDir :: IO FilePath
sshDir = do
	home <- myHomeDir
	return $ home </> ".ssh"
