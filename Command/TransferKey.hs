{- git-annex plumbing command (for use by old assistant, and users)
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
	<*> (AssociatedFile <$> optional (strOption
		( long "file" <> metavar paramFile
		<> help "the associated file"
		)))

instance DeferredParseClass TransferKeyOptions where
	finishParse v = TransferKeyOptions
		<$> pure (keyOptions v)
		<*> finishParse (fromToOptions v)
		<*> pure (fileOption v)

seek :: TransferKeyOptions -> CommandSeek
seek o = withKeys (commandAction . start o) (keyOptions o)

start :: TransferKeyOptions -> (SeekInput, Key) -> CommandStart
start o (_, key) = startingCustomOutput key $ case fromToOptions o of
	ToRemote dest -> toPerform key (fileOption o) =<< getParsed dest
	FromRemote src -> fromPerform key (fileOption o) =<< getParsed src

toPerform :: Key -> AssociatedFile -> Remote -> CommandPerform
toPerform key file remote = go Upload file $
	upload (uuid remote) key file stdRetry $ \p -> do
		tryNonAsync (Remote.storeKey remote key file p) >>= \case
			Right () -> do
				Remote.logStatus remote key InfoPresent
				return True
			Left e -> do
				warning (show e)
				return False

fromPerform :: Key -> AssociatedFile -> Remote -> CommandPerform
fromPerform key file remote = go Upload file $
	download (uuid remote) key file stdRetry $ \p ->
		getViaTmp (retrievalSecurityPolicy remote) (RemoteVerify remote) key file $ \t ->
			tryNonAsync (Remote.retrieveKeyFile remote key file (fromRawFilePath t) p) >>= \case
				Right v -> return (True, v)	
				Left e -> do
					warning (show e)
					return (False, UnVerified)

go :: Direction -> AssociatedFile -> (NotifyWitness -> Annex Bool) -> CommandPerform
go direction file a = notifyTransfer direction file a >>= liftIO . exitBool
