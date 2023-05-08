{- git-annex command
 -
 - Copyright 2019-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.RenameRemote where

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
	command "renameremote" SectionSetup
		"changes name of special remote"
		(paramPair paramName paramName)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start ps@(oldname:newname:[]) = Annex.SpecialRemote.findExisting oldname >>= \case
	((u, cfg, mcu):[]) -> Annex.SpecialRemote.findExisting newname >>= \case
		[] -> go u cfg mcu
		_ -> giveup $ "The name " ++ newname ++ " is already used by a special remote."
	-- Support lookup by uuid or description as well as remote name,
	-- as a fallback when there is nothing with the name in the
	-- special remote log.
	[] -> Remote.nameToUUID' oldname >>= \case
		([u], _) -> do
			m <- Logs.Remote.remoteConfigMap
			case M.lookup u m of
				Nothing -> giveup "That is not a special remote."
				Just cfg -> go u cfg Nothing
		(_, msg) -> giveup msg
	_ -> giveup $ "There are multiple special remotes named " ++ oldname ++ ". Provide instead the uuid or description of the remote to rename."
  where
	ai = ActionItemOther Nothing
	si = SeekInput ps
	go u cfg mcu = starting "rename" ai si $ perform u cfg mcu newname
start _ = giveup "Specify an old name (or uuid or description) and a new name."

perform :: UUID -> R.RemoteConfig -> Maybe (Annex.SpecialRemote.ConfigFrom UUID) -> String -> CommandPerform
perform u cfg mcu newname = do
	let (namefield, cu) = case mcu of
		Nothing -> (nameField, u)
		Just (Annex.SpecialRemote.ConfigFrom u') -> (sameasNameField, u')
	Logs.Remote.configSet cu (M.insert namefield (Proposed newname) cfg)
	
	next $ return True
