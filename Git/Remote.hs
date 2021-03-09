{- git remote stuff
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Remote where

import Common
import Git
import Git.Types

import Data.Char
import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Network.URI
#ifdef mingw32_HOST_OS
import Git.FilePath
#endif

{- Is a git config key one that specifies the location of a remote? -}
isRemoteKey :: ConfigKey -> Bool
isRemoteKey (ConfigKey k) = "remote." `S.isPrefixOf` k && ".url" `S.isSuffixOf` k

{- Get a remote's name from the config key that specifies its location. -}
remoteKeyToRemoteName :: ConfigKey -> RemoteName
remoteKeyToRemoteName (ConfigKey k) = decodeBS' $
	S.intercalate "." $ dropFromEnd 1 $ drop 1 $ S8.split '.' k

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
	
data RemoteLocation = RemoteUrl String | RemotePath FilePath
	deriving (Eq)

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
		| otherwise = replacement ++ drop (S.length bestvalue) l
	  where
		replacement = decodeBS' $ S.drop (S.length prefix) $
			S.take (S.length bestkey - S.length suffix) bestkey
		(bestkey, bestvalue) = 
			case maximumBy longestvalue insteadofs of
				(ConfigKey k, ConfigValue v) -> (k, v)
				(ConfigKey k, NoConfigValue) -> (k, mempty)
		longestvalue (_, a) (_, b) = compare b a
		insteadofs = filterconfig $ \case
			(ConfigKey k, ConfigValue v) -> 
				prefix `S.isPrefixOf` k &&
				suffix `S.isSuffixOf` k &&
				v `S.isPrefixOf` encodeBS l
			(_, NoConfigValue) -> False
		filterconfig f = filter f $
			concatMap splitconfigs $ M.toList $ fullconfig repo
		splitconfigs (k, vs) = map (\v -> (k, v)) vs
		(prefix, suffix) = ("url." , ".insteadof")
	-- git supports URIs that contain unescaped characters such as
	-- spaces. So to test if it's a (git) URI, escape those.
	urlstyle v = isURI (escapeURIString isUnescapedInURI v)
	-- git remotes can be written scp style -- [user@]host:dir
	-- but foo::bar is a git-remote-helper location instead
	scpstyle v = ":" `isInfixOf` v 
		&& not ("//" `isInfixOf` v)
		&& not ("::" `isInfixOf` v)
	scptourl v = "ssh://" ++ host ++ slash dir
	  where
		(host, dir)
			-- handle ipv6 address inside []
			| "[" `isPrefixOf` v = case break (== ']') v of
				(h, ']':':':d) -> (h ++ "]", d)
				(h, ']':d) -> (h ++ "]", d)
				(h, d) -> (h, d)
			| otherwise = separate (== ':') v
		slash d	| d == "" = "/~/" ++ d
			| "/" `isPrefixOf` d = d
			| "~" `isPrefixOf` d = '/':d
			| otherwise = "/~/" ++ d
#ifdef mingw32_HOST_OS
	-- git on Windows will write a path to .git/config with "drive:",
	-- which is not to be confused with a "host:"
	dosstyle = hasDrive
	dospath = fromRawFilePath . fromInternalGitPath . toRawFilePath
#endif
