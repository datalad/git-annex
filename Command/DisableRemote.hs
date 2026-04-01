{- git-annex command
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.DisableRemote where

import Command
import qualified Annex.SpecialRemote
import Annex.SpecialRemote.Config (nameField, sameasNameField)
import qualified Logs.Remote
import qualified Types.Remote as R
import qualified Remote
import Types.ProposedAccepted

import qualified Data.Map as M

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "disableremote" SectionSetup
		"stops git-annex from using a remote"
		paramName
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start ps@(name:[]) = error "TODO"
start _ = giveup "Specify the remote's name."
