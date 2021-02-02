{- git-annex global options
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
  -}
  
module CmdLine.GlobalSetter where

import Types.DeferredParse
import Common
import Annex

import Options.Applicative

globalFlag :: Annex () -> Mod FlagFields GlobalSetter -> GlobalOption
globalFlag setter = flag' (DeferredParse setter) 

globalSetter :: (v -> Annex ()) -> Parser v -> GlobalOption
globalSetter setter parser = DeferredParse . setter <$> parser

parserGlobalOptions :: [GlobalOption] -> Parser GlobalSetter
parserGlobalOptions [] = DeferredParse <$> pure noop
parserGlobalOptions l = DeferredParse . mapM_ getParsed
	<$> many (foldl1 (<|>) l)
