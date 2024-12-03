{- git remote stuff
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Remote where

import Common
import Git
import Git.Types
import Git.Command

import Data.Char
import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.List.NonEmpty as NE
import Network.URI
#ifdef mingw32_HOST_OS
import Git.FilePath
#endif

{- Lists all currently existing git remotes. -}
listRemotes :: Repo -> IO [RemoteName]
listRemotes repo = map decodeBS . S8.lines
	<$> pipeReadStrict [Param "remote"] repo

{- Is a git config key one that specifies the url of a remote? -}
isRemoteUrlKey :: ConfigKey -> Bool
isRemoteUrlKey = isRemoteKey "url"

isRemoteKey :: S.ByteString -> ConfigKey -> Bool
isRemoteKey want (ConfigKey k) =
	"remote." `S.isPrefixOf` k && ("." <> want) `S.isSuffixOf` k

{- Get a remote's name from the a config key such as remote.name.url
 - or any other per-remote config key. -}
remoteKeyToRemoteName :: ConfigKey -> Maybe RemoteName
remoteKeyToRemoteName (ConfigKey k)
	| "remote." `S.isPrefixOf` k = 
		let n = S.intercalate "." $ dropFromEnd 1 $ drop 1 $ S8.split '.' k
		in if S.null n then Nothing else Just (decodeBS n)
	| otherwise = Nothing

{- Construct a legal git remote name out of an arbitrary input string.
 -
 - There seems to be no formal definition of this in the git source,
 - just some ad-hoc checks, and some other things that fail with certain
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
	legal '-' = True
	legal '.' = True
	legal c = isAlphaNum c

isLegalName :: String -> Bool
isLegalName s = s == makeLegalName s

data RemoteLocation = RemoteUrl String | RemotePath FilePath
	deriving (Eq, Show)

remoteLocationIsUrl :: RemoteLocation -> Bool
remoteLocationIsUrl (RemoteUrl _) = True
remoteLocationIsUrl _ = False

remoteLocationIsSshUrl :: RemoteLocation -> Bool
remoteLocationIsSshUrl (RemoteUrl u) = "ssh://" `isPrefixOf` u
remoteLocationIsSshUrl _ = False

{- Determines if a given remote location is an url, or a local
 - path. Takes the repository's insteadOf configuration into account. -}
parseRemoteLocation :: String -> Bool -> Repo -> RemoteLocation
parseRemoteLocation s knownurl repo = go
  where
 	s' = fromMaybe s $ insteadOfUrl s ".insteadof" $ fullconfig repo
	go
#ifdef mingw32_HOST_OS
		| dosstyle s' = RemotePath (dospath s')
#endif
		| scpstyle s' = RemoteUrl (scptourl s')
		| urlstyle s' = RemoteUrl s'
		| knownurl && s' == s = RemoteUrl s'
		| otherwise = RemotePath s'
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

insteadOfUrl :: String -> S.ByteString -> RepoFullConfig -> Maybe String
insteadOfUrl u configsuffix fullcfg
	| null insteadofs = Nothing
	| otherwise = Just $ replacement ++ drop (S.length bestvalue) u
  where
	replacement = decodeBS $ S.drop (S.length configprefix) $
		S.take (S.length bestkey - S.length configsuffix) bestkey
	(bestkey, bestvalue) = 
		case maximumBy longestvalue insteadofs of
			(ConfigKey k, ConfigValue v) -> (k, v)
			(ConfigKey k, NoConfigValue) -> (k, mempty)
	longestvalue (_, a) (_, b) = compare b a
	insteadofs = filterconfig $ \case
		(ConfigKey k, ConfigValue v) -> 
			configprefix `S.isPrefixOf` k &&
			configsuffix `S.isSuffixOf` k &&
			v `S.isPrefixOf` encodeBS u
		(_, NoConfigValue) -> False
	filterconfig f = filter f $
		concatMap splitconfigs $ M.toList fullcfg
	splitconfigs (k, vs) = map (\v -> (k, v)) (NE.toList vs)
	configprefix = "url."
