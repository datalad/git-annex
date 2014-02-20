{- git-annex command
 -
 - Copyright 2010, 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Trust where

import Common.Annex
import Command
import qualified Remote
import Types.TrustLevel
import Logs.Trust
import Logs.Group

import qualified Data.Set as S

def :: [Command]
def = [command "trust" (paramRepeating paramRemote) seek
	SectionSetup "trust a repository"]

seek :: CommandSeek
seek = trustCommand "trust" Trusted

trustCommand :: String -> TrustLevel -> CommandSeek
trustCommand cmd level = withWords start
  where
	start ws = do
		let name = unwords ws
		showStart cmd name
		u <- Remote.nameToUUID name
		next $ perform u
	perform uuid = do
		trustSet uuid level
		when (level == DeadTrusted) $
			groupSet uuid S.empty
		l <- lookupTrust uuid
		when (l /= level) $
			warning $ "This remote's trust level is locally overridden to " ++ showTrustLevel l ++ " via git config."
		next $ return True
