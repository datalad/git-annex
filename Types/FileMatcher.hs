{- git-annex file matcher types
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.FileMatcher where

import Types.UUID (UUID)
import Types.Key (Key, AssociatedFile)
import Types.Mime
import Utility.Matcher (Matcher, Token)
import Utility.FileSize
import Utility.FileSystemEncoding

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S

-- Information about a file or a key that can be matched on.
data MatchInfo
	= MatchingFile FileInfo
	| MatchingKey Key AssociatedFile
	| MatchingInfo ProvidedInfo
	| MatchingUserInfo UserProvidedInfo

data FileInfo = FileInfo
	{ contentFile :: Maybe RawFilePath
	-- ^ path to a file containing the content, for operations
	-- that examine it
	, matchFile :: RawFilePath
	-- ^ filepath to match on; may be relative to top of repo or cwd,
	-- depending on how globs in preferred content expressions
	-- are intended to be matched
	}

data ProvidedInfo = ProvidedInfo
	{ providedFilePath :: RawFilePath
	, providedKey :: Maybe Key
	, providedFileSize :: FileSize
	, providedMimeType :: Maybe MimeType
	, providedMimeEncoding :: Maybe MimeEncoding
	}

-- This is used when testing a matcher, with values to match against
-- provided by the user.
data UserProvidedInfo = UserProvidedInfo
	{ userProvidedFilePath :: UserInfo FilePath
	, userProvidedKey :: UserInfo Key
	, userProvidedFileSize :: UserInfo FileSize
	, userProvidedMimeType :: UserInfo MimeType
	, userProvidedMimeEncoding :: UserInfo MimeEncoding
	}

-- This may fail if the user did not provide the information.
type UserInfo a = Either (IO a) a

-- If the UserInfo is not available, accessing it may result in eg an
-- exception being thrown.
getUserInfo :: MonadIO m => UserInfo a -> m a
getUserInfo (Right i) = return i
getUserInfo (Left e) = liftIO e

type FileMatcherMap a = M.Map UUID (FileMatcher a)

type MkLimit a = String -> Either String (MatchFiles a)

type AssumeNotPresent = S.Set UUID

data MatchFiles a = MatchFiles 
	{ matchAction :: AssumeNotPresent -> MatchInfo -> a Bool
	, matchNeedsFileName :: Bool
	-- ^ does the matchAction need a filename in order to match?
	, matchNeedsFileContent :: Bool
	-- ^ does the matchAction need the file content to be present in
	-- order to succeed?
	, matchNeedsKey :: Bool
	-- ^ does the matchAction look at information about the key?
	, matchNeedsLocationLog :: Bool
	-- ^ does the matchAction look at the location log?
	}

type FileMatcher a = Matcher (MatchFiles a)

-- This is a matcher that can have tokens added to it while it's being
-- built, and once complete is compiled to an unchangable matcher.
data ExpandableMatcher a
	= BuildingMatcher [Token (MatchFiles a)]
	| CompleteMatcher (Matcher (MatchFiles a))
