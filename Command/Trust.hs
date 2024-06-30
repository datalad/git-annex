{- git-annex command
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
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
cmd = withAnnexOptions [jsonOptions] $
	command "trust" SectionSetup "trust a repository"
		(paramRepeating paramRepository)
		(withParams' seek completeRemotes)

seek :: CmdParams -> CommandSeek
seek = trustCommand "trust" Trusted

trustCommand :: String -> TrustLevel -> CmdParams -> CommandSeek
trustCommand _ _ [] = giveup "no repository name specified"
trustCommand c level ps = withStrings (commandAction . start) ps
  where
	start name = do
		u <- Remote.nameToUUID name
		let si = SeekInput [name]
		starting c (ActionItemUUID u (UnquotedString name)) si (perform name u)
	perform name uuid = do
		when (level >= Trusted) $
			unlessM (Annex.getRead Annex.force) $
				giveup $ trustedNeedsForce name
		trustSet uuid level
		when (level == DeadTrusted) $
			groupSet uuid S.empty
		l <- lookupTrust uuid
		when (l /= level) $
			warning $ UnquotedString $ "This remote's trust level is overridden to " ++ showTrustLevel l ++ "."
		next $ return True

trustedNeedsForce :: String -> String
trustedNeedsForce name = unwords
	[ "Trusting a repository can lead to data loss."
	, "If you're sure you know what you're doing, use --force to"
	, "make this take effect."
	, "If you choose to do so, bear in mind that any time you drop"
	, "content from " ++ name ++ ", you will risk losing data."
	]
