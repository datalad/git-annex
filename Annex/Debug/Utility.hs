{- git-annex debugging, utility functions
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Debug.Utility (
	debugSelectorFromGitConfig,
	parseDebugSelector,
	DebugSelector,
) where

import Types.GitConfig
import Utility.Debug
import Utility.Split
import Utility.FileSystemEncoding

import qualified Data.ByteString as S

debugSelectorFromGitConfig :: GitConfig -> DebugSelector
debugSelectorFromGitConfig = 
	maybe NoDebugSelector parseDebugSelector . annexDebugFilter

parseDebugSelector :: String -> DebugSelector
parseDebugSelector = DebugSelector . matchDebugSource . splitSelectorNames

splitSelectorNames :: String -> [S.ByteString]
splitSelectorNames = map encodeBS . splitc ','

matchDebugSource :: [S.ByteString] -> DebugSource -> Bool
matchDebugSource names (DebugSource s) = any (`S.isInfixOf` s) names
