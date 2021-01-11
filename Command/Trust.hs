{- git-annex command
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Trust where

import Command
import qualified Remote
import qualified Annex
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
		let si = SeekInput ws
		starting c (ActionItemOther (Just name)) si (perform name u)
	perform name uuid = do
		when (level >= Trusted) $
			unlessM (Annex.getState Annex.force) $
				giveup $ trustedNeedsForce name
		trustSet uuid level
		when (level == DeadTrusted) $
			groupSet uuid S.empty
		l <- lookupTrust uuid
		when (l /= level) $
			warning $ "This remote's trust level is overridden to " ++ showTrustLevel l ++ "."
		next $ return True

trustedNeedsForce :: String -> String
trustedNeedsForce name = unwords
	[ "Trusting a repository can lead to data loss."
	, "If you're sure you know what you're doing, use --force to"
	, "make this take effect."
	, "If you choose to do so, bear in mind that any time you drop"
	, "content from " ++ name ++ ", you will risk losing data."
	]
