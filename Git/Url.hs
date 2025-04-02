{- git repository urls
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.Url (
	scheme,
	host,
	port,
	hostuser,
	authority,
	path,
) where

import Network.URI hiding (scheme, authority, path)

import Common
import Git.Types

{- Scheme of an URL repo. -}
scheme :: Repo -> Maybe String
scheme Repo { location = Url u } = Just (uriScheme u)
scheme _ = Nothing

{- Work around a bug in the real uriRegName
 - <http://trac.haskell.org/network/ticket/40> -}
uriRegName' :: URIAuth -> String
uriRegName' a = fixup $ uriRegName a
  where
	fixup x@('[':rest)
		| rest !! len == ']' = take len rest
		| otherwise = x
	  where
		len  = length rest - 1
	fixup x = x

{- Hostname of an URL repo. 
 -
 - An IPV6 link-local address in an url can include a
 - scope, eg "%wlan0". The "%" is necessarily URI-encoded
 - as "%25" in the URI. So the hostname gets URI-decoded here.
 -}
host :: Repo -> Maybe String
host = authpart (unEscapeString . uriRegName')

{- Port of an URL repo, if it has a nonstandard one. -}
port :: Repo -> Maybe Integer
port r = 
	case authpart uriPort r of
		Nothing -> Nothing
		Just ":" -> Nothing
		Just (':':p) -> readish p
		Just _ -> Nothing

{- Hostname of an URL repo, including any username (ie, "user@host")
 -
 - Both the username and hostname are URI-decoded.
 -}
hostuser :: Repo -> Maybe String
hostuser r = (++)
	<$> authpart (unEscapeString . uriUserInfo) r
	<*> host r

{- The full authority portion an URL repo. (ie, "user@host:port") -}
authority :: Repo -> Maybe String
authority = authpart assemble
  where
	assemble a = uriUserInfo a ++ uriRegName' a ++ uriPort a

{- Applies a function to extract part of the uriAuthority of an URL repo. -}
authpart :: (URIAuth -> a) -> Repo -> Maybe a
authpart a Repo { location = Url u } = a <$> uriAuthority u
authpart _ _ = Nothing

{- Path part of an URL repo. It is URI-decoded. -}
path :: Repo -> Maybe FilePath
path Repo { location = Url u } = Just $ unEscapeString $ uriPath u
path _ = Nothing
