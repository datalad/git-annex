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
toPerform key af remote = go Upload af $
	upload' (uuid remote) key af Nothing stdRetry $ \p -> do
		tryNonAsync (Remote.storeKey remote key af Nothing p) >>= \case
			Right () -> do
				Remote.logStatus NoLiveUpdate remote key InfoPresent
				return True
			Left e -> do
				warning (UnquotedString (show e))
				return False

fromPerform :: Key -> AssociatedFile -> Remote -> CommandPerform
fromPerform key af remote = go Upload af $
	download' (uuid remote) key af Nothing stdRetry $ \p ->
		logStatusAfter NoLiveUpdate key $ getViaTmp (retrievalSecurityPolicy remote) vc key af Nothing $ \t ->
			tryNonAsync (Remote.retrieveKeyFile remote key af (fromRawFilePath t) p vc) >>= \case
				Right v -> return (True, v)	
				Left e -> do
					warning (UnquotedString (show e))
					return (False, UnVerified)
  where
	vc = RemoteVerify remote

go :: Direction -> AssociatedFile -> (NotifyWitness -> Annex Bool) -> CommandPerform
go direction file a = notifyTransfer direction file a >>= liftIO . exitBool
