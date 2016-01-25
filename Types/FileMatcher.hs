{- git-annex file matcher types
 -
 - Copyright 2013-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.FileMatcher where

import Types.UUID (UUID)
import Types.Key (Key)
import Utility.Matcher (Matcher, Token)
import Utility.FileSize

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S

data MatchInfo
	= MatchingFile FileInfo
	| MatchingKey Key
	| MatchingInfo (OptInfo FilePath) (OptInfo Key) (OptInfo FileSize)

data FileInfo = FileInfo
	{ currFile :: FilePath
	-- ^ current path to the file, for operations that examine it
	, matchFile :: FilePath
	-- ^ filepath to match on; may be relative to top of repo or cwd
	}

type OptInfo a = Either (IO a) a

-- If the OptInfo is not available, accessing it may result in eg an
-- exception being thrown.
getInfo :: MonadIO m => OptInfo a -> m a
getInfo (Right i) = pure i
getInfo (Left e) = liftIO e

type FileMatcherMap a = M.Map UUID (Utility.Matcher.Matcher (S.Set UUID -> MatchInfo -> a Bool))

type MkLimit a = String -> Either String (MatchFiles a)

type AssumeNotPresent = S.Set UUID

type MatchFiles a = AssumeNotPresent -> MatchInfo -> a Bool

type FileMatcher a = Matcher (MatchFiles a)

-- This is a matcher that can have tokens added to it while it's being
-- built, and once complete is compiled to an unchangable matcher.
data ExpandableMatcher a
	= BuildingMatcher [Token (MatchInfo -> a Bool)]
	| CompleteMatcher (Matcher (MatchInfo -> a Bool))
