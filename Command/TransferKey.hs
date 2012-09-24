{- git-annex command, used internally by assistant
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TransferKey where

import Common.Annex
import Command
import Annex.Content
import Logs.Location
import Logs.Transfer
import qualified Remote
import Types.Remote
import qualified Command.Move
import qualified Option

def :: [Command]
def = [withOptions options $
	noCommit $ command "transferkey" paramKey seek
		"transfers a key from or to a remote"]

options :: [Option]
options = fileOption : Command.Move.options

fileOption :: Option
fileOption = Option.field [] "file" paramFile "the associated file"

seek :: [CommandSeek]
seek = [withField Command.Move.toOption Remote.byName $ \to ->
	withField Command.Move.fromOption Remote.byName $ \from ->
	withField fileOption return $ \file ->
		withKeys $ start to from file]

start :: Maybe Remote -> Maybe Remote -> AssociatedFile -> Key -> CommandStart
start to from file key =
	case (from, to) of
		(Nothing, Just dest) -> next $ toPerform dest key file
		(Just src, Nothing) -> next $ fromPerform src key file
		_ -> error "specify either --from or --to"

toPerform :: Remote -> Key -> AssociatedFile -> CommandPerform
toPerform remote key file = go $
	upload (uuid remote) key file forwardRetry $ \p -> do
		ok <- Remote.storeKey remote key file p
		when ok $
			Remote.logStatus remote key InfoPresent
		return ok

fromPerform :: Remote -> Key -> AssociatedFile -> CommandPerform
fromPerform remote key file = go $
	download (uuid remote) key file forwardRetry $
		getViaTmp key $ Remote.retrieveKeyFile remote key file

go :: Annex Bool -> CommandPerform
go a = ifM a ( liftIO exitSuccess,  liftIO exitFailure)
