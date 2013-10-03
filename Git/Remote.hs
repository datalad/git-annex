{- git remote stuff
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Remote where

import Common
import Git
import qualified Git.Command
import qualified Git.BuildVersion

import Data.Char

type RemoteName = String

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
