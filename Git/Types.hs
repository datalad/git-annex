{- git data types
 -
 - Copyright 2010,2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Types where

import Network.URI
import qualified Data.Map as M

{- There are two types of repositories; those on local disk and those
 - accessed via an URL. -}
data RepoLocation = Dir FilePath | Url URI | Unknown
	deriving (Show, Eq)

data Repo = Repo {
	location :: RepoLocation,
	config :: M.Map String String,
	-- a given git config key can actually have multiple values
	fullconfig :: M.Map String [String],
	remotes :: [Repo],
	-- remoteName holds the name used for this repo in remotes
	remoteName :: Maybe String 
} deriving (Show, Eq)

{- A git ref. Can be a sha1, or a branch or tag name. -}
newtype Ref = Ref String
	deriving (Eq)

instance Show Ref where
	show (Ref v) = v

{- Aliases for Ref. -}
type Branch = Ref
type Sha = Ref
type Tag = Ref
