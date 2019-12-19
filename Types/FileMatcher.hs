{- git-annex file matcher types
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
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

data FileInfo = FileInfo
	{ currFile :: RawFilePath
	-- ^ current path to the file, for operations that examine it
	, matchFile :: RawFilePath
	-- ^ filepath to match on; may be relative to top of repo or cwd
	}

-- This is used when testing a matcher, with values to match against
-- provided in some way, rather than queried from files on disk.
data ProvidedInfo = ProvidedInfo
	{ providedFilePath :: OptInfo FilePath
	, providedKey :: OptInfo Key
	, providedFileSize :: OptInfo FileSize
	, providedMimeType :: OptInfo MimeType
	, providedMimeEncoding :: OptInfo MimeEncoding
	}

type OptInfo a = Either (IO a) a

-- If the OptInfo is not available, accessing it may result in eg an
-- exception being thrown.
getInfo :: MonadIO m => OptInfo a -> m a
getInfo (Right i) = return i
getInfo (Left e) = liftIO e

type FileMatcherMap a = M.Map UUID (FileMatcher a)

type MkLimit a = String -> Either String (MatchFiles a)

type AssumeNotPresent = S.Set UUID

type MatchFiles a = AssumeNotPresent -> MatchInfo -> a Bool

type FileMatcher a = Matcher (MatchFiles a)

-- This is a matcher that can have tokens added to it while it's being
-- built, and once complete is compiled to an unchangable matcher.
data ExpandableMatcher a
	= BuildingMatcher [Token (MatchInfo -> a Bool)]
	| CompleteMatcher (Matcher (MatchInfo -> a Bool))
