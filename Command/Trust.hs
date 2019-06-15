{- git-annex command
 -
 - Copyright 2010, 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Trust where

import Command
import qualified Remote
import Types.TrustLevel
import Logs.Trust
import Logs.Group

import qualified Data.Set as S

cmd :: Command
cmd = command "trust" SectionSetup "trust a repository"
	(paramRepeating paramRemote) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = trustCommand "trust" Trusted

trustCommand :: String -> TrustLevel -> CmdParams -> CommandSeek
trustCommand c level = withWords (commandAction . start)
  where
	start ws = do
		let name = unwords ws
		u <- Remote.nameToUUID name
		starting c (ActionItemOther (Just name)) (perform u)
	perform uuid = do
		trustSet uuid level
		when (level == DeadTrusted) $
			groupSet uuid S.empty
		l <- lookupTrust uuid
		when (l /= level) $
			warning $ "This remote's trust level is overridden to " ++ showTrustLevel l ++ "."
		next $ return True
