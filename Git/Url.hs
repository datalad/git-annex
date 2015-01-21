{- git repository urls
 -
 - Copyright 2010, 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Url (
	scheme,
	host,
	port,
	hostuser,
	authority,
) where

import Network.URI hiding (scheme, authority)

import Common
import Git.Types
import Git

{- Scheme of an URL repo. -}
scheme :: Repo -> String
scheme Repo { location = Url u } = uriScheme u
scheme repo = notUrl repo

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

{- Hostname of an URL repo. -}
host :: Repo -> Maybe String
host = authpart uriRegName'

{- Port of an URL repo, if it has a nonstandard one. -}
port :: Repo -> Maybe Integer
port r = 
	case authpart uriPort r of
		Nothing -> Nothing
		Just ":" -> Nothing
		Just (':':p) -> readish p
		Just _ -> Nothing

{- Hostname of an URL repo, including any username (ie, "user@host") -}
hostuser :: Repo -> Maybe String
hostuser r = (++)
	<$> authpart uriUserInfo r
	<*> authpart uriRegName' r

{- The full authority portion an URL repo. (ie, "user@host:port") -}
authority :: Repo -> Maybe String
authority = authpart assemble
  where
	assemble a = uriUserInfo a ++ uriRegName' a ++ uriPort a

{- Applies a function to extract part of the uriAuthority of an URL repo. -}
authpart :: (URIAuth -> a) -> Repo -> Maybe a
authpart a Repo { location = Url u } = a <$> uriAuthority u
authpart _ repo = notUrl repo

notUrl :: Repo -> a
notUrl repo = error $
	"acting on local git repo " ++ repoDescribe repo ++ " not supported"
