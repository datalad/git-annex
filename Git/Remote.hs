{- git remote stuff
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Git.Remote where

import Common
import Git
import Git.Types
import qualified Git.Command
import qualified Git.BuildVersion

import Data.Char
import qualified Data.Map as M
import Network.URI
#ifdef mingw32_HOST_OS
import Git.FilePath
#endif

{- Construct a legal git remote name out of an arbitrary input string.
 -
 - There seems to be no formal definition of this in the git source,
 - just some ad-hoc checks, and some other things that fail with certian
 - types of names (like ones starting with '-').
 -}
makeLegalName :: String -> RemoteName
makeLegalName s = case filter legal $ replace "/" "_" s of
	-- it can't be empty
	[] -> "unnamed"
	-- it can't start with / or - or .
	'.':s' -> makeLegalName s'
	'/':s' -> makeLegalName s'
	'-':s' -> makeLegalName s'
	s' -> s'
  where
	{- Only alphanumerics, and a few common bits of punctuation common
	 - in hostnames. -}
	legal '_' = True
	legal '.' = True
	legal c = isAlphaNum c
	
remove :: RemoteName -> Repo -> IO ()
remove remotename = Git.Command.run
	[ Param "remote"
	-- name of this subcommand changed
	, Param $
		if Git.BuildVersion.older "1.8.0"
			then "rm"
			else "remove"
	, Param remotename
	]

data RemoteLocation = RemoteUrl String | RemotePath FilePath

remoteLocationIsUrl :: RemoteLocation -> Bool
remoteLocationIsUrl (RemoteUrl _) = True
remoteLocationIsUrl _ = False

remoteLocationIsSshUrl :: RemoteLocation -> Bool
remoteLocationIsSshUrl (RemoteUrl u) = "ssh://" `isPrefixOf` u
remoteLocationIsSshUrl _ = False

{- Determines if a given remote location is an url, or a local
 - path. Takes the repository's insteadOf configuration into account. -}
parseRemoteLocation :: String -> Repo -> RemoteLocation
parseRemoteLocation s repo = ret $ calcloc s
  where
  	ret v
#ifdef mingw32_HOST_OS
		| dosstyle v = RemotePath (dospath v)
#endif
		| scpstyle v = RemoteUrl (scptourl v)
		| urlstyle v = RemoteUrl v
		| otherwise = RemotePath v
	-- insteadof config can rewrite remote location
	calcloc l
		| null insteadofs = l
		| otherwise = replacement ++ drop (length bestvalue) l
	  where
		replacement = drop (length prefix) $
			take (length bestkey - length suffix) bestkey
		(bestkey, bestvalue) = maximumBy longestvalue insteadofs
		longestvalue (_, a) (_, b) = compare b a
		insteadofs = filterconfig $ \(k, v) -> 
			startswith prefix k &&
			endswith suffix k &&
			startswith v l
		filterconfig f = filter f $
			concatMap splitconfigs $ M.toList $ fullconfig repo
		splitconfigs (k, vs) = map (\v -> (k, v)) vs
		(prefix, suffix) = ("url." , ".insteadof")
	urlstyle v = isURI v || ":" `isInfixOf` v && "//" `isInfixOf` v
	-- git remotes can be written scp style -- [user@]host:dir
	-- but foo::bar is a git-remote-helper location instead
	scpstyle v = ":" `isInfixOf` v 
		&& not ("//" `isInfixOf` v)
		&& not ("::" `isInfixOf` v)
	scptourl v = "ssh://" ++ host ++ slash dir
	  where
		(host, dir) = separate (== ':') v
		slash d	| d == "" = "/~/" ++ d
			| "/" `isPrefixOf` d = d
			| "~" `isPrefixOf` d = '/':d
			| otherwise = "/~/" ++ d
#ifdef mingw32_HOST_OS
	-- git on Windows will write a path to .git/config with "drive:",
	-- which is not to be confused with a "host:"
	dosstyle = hasDrive
	dospath = fromInternalGitPath
#endif
