{- git-annex options that are stored in Annex
 -
 - Copyright 2015-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
  -}
  
module CmdLine.AnnexSetter where

import Common
import Annex
import Types.DeferredParse

import Options.Applicative

setAnnexState :: Annex () -> AnnexSetter
setAnnexState a = AnnexSetter a id

setAnnexRead :: (AnnexRead -> AnnexRead) -> AnnexSetter
setAnnexRead f = AnnexSetter (return ()) f

annexFlag :: AnnexSetter -> Mod FlagFields AnnexSetter -> AnnexOption
annexFlag = flag'

annexOption :: (v -> AnnexSetter) -> Parser v -> AnnexOption
annexOption mk parser = mk <$> parser

-- | Combines a bunch of AnnexOptions together into a Parser
-- that returns a AnnexSetter that can be used to set all the options that
-- are enabled.
parserAnnexOptions :: [AnnexOption] -> Parser AnnexSetter
parserAnnexOptions [] = pure mempty
parserAnnexOptions l = mconcat <$> many (foldl1 (<|>) l)

applyAnnexReadSetter :: AnnexSetter -> (AnnexState, AnnexRead) -> (AnnexState, AnnexRead)
applyAnnexReadSetter gs (st, rd) = (st, annexReadSetter gs rd)
