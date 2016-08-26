{- git-annex plumbing command (for use by old assistant, and users)
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TransferKey where

import Command
import Annex.Content
import Logs.Location
import Annex.Transfer
import qualified Remote
import Types.Remote

cmd :: Command
cmd = noCommit $
	command "transferkey" SectionPlumbing
		"transfers a key from or to a remote"
		paramKey (seek <--< optParser)

data TransferKeyOptions = TransferKeyOptions
	{ keyOptions :: CmdParams 
	, fromToOptions :: FromToOptions
	, fileOption :: AssociatedFile
	}

optParser :: CmdParamsDesc -> Parser TransferKeyOptions
optParser desc  = TransferKeyOptions
	<$> cmdParams desc
	<*> parseFromToOptions
	<*> optional (strOption
		( long "file" <> metavar paramFile
		<> help "the associated file"
		))

instance DeferredParseClass TransferKeyOptions where
	finishParse v = TransferKeyOptions
		<$> pure (keyOptions v)
		<*> finishParse (fromToOptions v)
		<*> pure (fileOption v)

seek :: TransferKeyOptions -> CommandSeek
seek o = withKeys (start o) (keyOptions o)

start :: TransferKeyOptions -> Key -> CommandStart
start o key = case fromToOptions o of
	ToRemote dest -> next $ toPerform key (fileOption o) =<< getParsed dest
	FromRemote src -> next $ fromPerform key (fileOption o) =<< getParsed src

toPerform :: Key -> AssociatedFile -> Remote -> CommandPerform
toPerform key file remote = go Upload file $
	upload (uuid remote) key file forwardRetry $ \p -> do
		ok <- Remote.storeKey remote key file p
		when ok $
			Remote.logStatus remote key InfoPresent
		return ok

fromPerform :: Key -> AssociatedFile -> Remote -> CommandPerform
fromPerform key file remote = go Upload file $
	download (uuid remote) key file forwardRetry $ \p ->
		getViaTmp (RemoteVerify remote) key $ 
			\t -> Remote.retrieveKeyFile remote key file t p

go :: Direction -> AssociatedFile -> (NotifyWitness -> Annex Bool) -> CommandPerform
go direction file a = notifyTransfer direction file a >>= liftIO . exitBool
