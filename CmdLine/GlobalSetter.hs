{- git-annex global options
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
  -}
  
module CmdLine.GlobalSetter where

import Common
import Annex
import Types.DeferredParse

import Options.Applicative

setAnnexState :: Annex () -> GlobalSetter
setAnnexState a = GlobalSetter a id

setAnnexRead :: (AnnexRead -> AnnexRead) -> GlobalSetter
setAnnexRead f = GlobalSetter (return ()) f

globalFlag :: GlobalSetter -> Mod FlagFields GlobalSetter -> GlobalOption
globalFlag = flag'

globalOption :: (v -> GlobalSetter) -> Parser v -> GlobalOption
globalOption mk parser = mk <$> parser

-- | Combines a bunch of GlobalOptions together into a Parser
-- that returns a GlobalSetter that can be used to set all the options that
-- are enabled.
parserGlobalOptions :: [GlobalOption] -> Parser GlobalSetter
parserGlobalOptions [] = pure mempty
parserGlobalOptions l = mconcat <$> many (foldl1 (<|>) l)

applyAnnexReadSetter :: GlobalSetter -> (AnnexState, AnnexRead) -> (AnnexState, AnnexRead)
applyAnnexReadSetter gs (st, rd) = (st, annexReadSetter gs rd)
