{- git-annex exports
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Export where

import Annex
import Annex.CatFile
import Types
import Types.Key
import qualified Git
import qualified Types.Remote as Remote
import Config
import Messages
import Utility.FileSystemEncoding

import qualified Data.Map as M
import Control.Applicative
import Data.Maybe
import Prelude

-- An export includes both annexed files and files stored in git.
-- For the latter, a SHA1 key is synthesized.
data ExportKey = AnnexKey Key | GitKey Key
	deriving (Show, Eq, Ord)

asKey :: ExportKey -> Key
asKey (AnnexKey k) = k
asKey (GitKey k) = k

exportKey :: Git.Sha -> Annex ExportKey
exportKey sha = mk <$> catKey sha
  where
	mk (Just k) = AnnexKey k
	mk Nothing = GitKey $ Key
		{ keyName = encodeBS $ Git.fromRef sha
		, keyVariety = SHA1Key (HasExt False)
		, keySize = Nothing
		, keyMtime = Nothing
		, keyChunkSize = Nothing
		, keyChunkNum = Nothing
		}

exportTree :: Remote.RemoteConfig -> Bool
exportTree c = fromMaybe False $ yesNo =<< M.lookup "exporttree" c

warnExportConflict :: Remote -> Annex ()
warnExportConflict r = toplevelWarning True $
	"Export conflict detected. Different trees have been exported to " ++ 
	Remote.name r ++ 
	". Use git-annex export to resolve this conflict."
