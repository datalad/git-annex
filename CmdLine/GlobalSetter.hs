{- git-annex global options
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
  -}
  
module CmdLine.GlobalSetter where

import Types.DeferredParse
import Common
import Annex

import Options.Applicative

globalFlag :: Annex () -> Mod FlagFields GlobalSetter -> Parser GlobalSetter
globalFlag setter = flag' (DeferredParse setter) 

globalSetter :: (v -> Annex ()) -> Parser v -> Parser GlobalSetter
globalSetter setter parser = DeferredParse . setter <$> parser

combineGlobalSetters :: [Parser GlobalSetter] -> Parser GlobalSetter
combineGlobalSetters l = DeferredParse . sequence_ . map getParsed
	<$> many (foldl1 (<|>) l)
