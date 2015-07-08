{- git-annex plumbing command (for use by old assistant, and users)
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TransferKey where

import Common.Annex
import Command
import Annex.Content
import Logs.Location
import Annex.Transfer
import qualified Remote
import Types.Remote

cmd :: Command
cmd = withOptions transferKeyOptions $ noCommit $
	command "transferkey" SectionPlumbing
		"transfers a key from or to a remote"
		paramKey (withParams seek)

transferKeyOptions :: [Option]
transferKeyOptions = fileOption : fromToOptions

fileOption :: Option
fileOption = fieldOption [] "file" paramFile "the associated file"

seek :: CmdParams -> CommandSeek
seek ps = do
	to <- getOptionField toOption Remote.byNameWithUUID
	from <- getOptionField fromOption Remote.byNameWithUUID
	file <- getOptionField fileOption return
	withKeys (start to from file) ps

start :: Maybe Remote -> Maybe Remote -> AssociatedFile -> Key -> CommandStart
start to from file key =
	case (from, to) of
		(Nothing, Just dest) -> next $ toPerform dest key file
		(Just src, Nothing) -> next $ fromPerform src key file
		_ -> error "specify either --from or --to"

toPerform :: Remote -> Key -> AssociatedFile -> CommandPerform
toPerform remote key file = go Upload file $
	upload (uuid remote) key file forwardRetry noObserver $ \p -> do
		ok <- Remote.storeKey remote key file p
		when ok $
			Remote.logStatus remote key InfoPresent
		return ok

fromPerform :: Remote -> Key -> AssociatedFile -> CommandPerform
fromPerform remote key file = go Upload file $
	download (uuid remote) key file forwardRetry noObserver $ \p ->
		getViaTmp key $ \t -> Remote.retrieveKeyFile remote key file t p

go :: Direction -> AssociatedFile -> (NotifyWitness -> Annex Bool) -> CommandPerform
go direction file a = notifyTransfer direction file a >>= liftIO . exitBool
