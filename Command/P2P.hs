{- git-annex command
 -
 - Copyright 2016-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.P2P where

import Command
import P2P.Address
import P2P.Auth
import P2P.IO
import P2P.Generic
import qualified P2P.Protocol as P2P
import Git.Types
import qualified Git.Remote
import qualified Git.Command
import qualified Annex
import Annex.UUID
import Config
import Utility.AuthToken
import Utility.Tmp.Dir
import Utility.FileMode
import Utility.ThreadScheduler
import Utility.SafeOutput
import qualified Utility.FileIO as F
import qualified Utility.MagicWormhole as Wormhole
import qualified Command.EnableTor as EnableTor

import Control.Concurrent.Async
import qualified Data.Text as T

cmd :: Command
cmd = command "p2p" SectionSetup
	"configure peer-2-peer links between repositories"
	paramNothing (seek <$$> optParser)

data P2POpts
	= GenAddresses
	| LinkRemote
	| Pair
	| Enable P2PNetName

optParser :: CmdParamsDesc -> Parser (P2POpts, Maybe RemoteName)
optParser _ = (,)
	<$> (pair <|> linkremote <|> genaddresses <|> enable)
	<*> optional name
  where
	genaddresses = flag' GenAddresses
		( long "gen-addresses"
		<> help "generate addresses that allow accessing this repository over P2P networks"
		)
	linkremote = flag' LinkRemote
		( long "link"
		<> help "set up a P2P link to a git remote"
		)
	pair = flag' Pair
		( long "pair"
		<> help "pair with another repository"
		)
	enable = Enable . P2PNetName <$> strOption
                ( long "enable" <> metavar paramName
                <> help "enable using a P2P network"
                )
	name = Git.Remote.makeLegalName <$> strOption
		( long "name"
		<> metavar paramName
		<> help "name of remote"
		)

seek :: (P2POpts, Maybe RemoteName) -> CommandSeek
seek (GenAddresses, _) = genAddresses =<< loadP2PAddresses
seek (LinkRemote, Just name) = commandAction $
	linkRemote name
seek (LinkRemote, Nothing) = commandAction $
	linkRemote =<< unusedPeerRemoteName
seek (Pair, Just name) = commandAction $
	startPairing name =<< loadP2PAddresses
seek (Pair, Nothing) = commandAction $ do
	name <- unusedPeerRemoteName
	startPairing name =<< loadP2PAddresses
seek (Enable netname, _) = commandAction $
	enableNetwork netname

unusedPeerRemoteName :: Annex RemoteName
unusedPeerRemoteName = go (1 :: Integer) =<< usednames
  where
	usednames = mapMaybe remoteName <$> Annex.getGitRemotes
	go n names = do
		let name = "peer" ++ show n
		if name `elem` names
			then go (n+1) names
			else return name

-- Only addresses are output to stdout, to allow scripting.
genAddresses :: [P2PAddress] -> Annex ()
genAddresses [] = giveup "No P2P networks are currently available."
genAddresses addrs = do
	addrauths <- forM addrs go
	earlyWarning "These addresses allow access to this git-annex repository. Only share them with people you trust with that access, using trusted communication channels!"
	liftIO $ putStr $ safeOutput $ unlines $
		map formatP2PAddress addrauths
	
  where
	go addr = do
		authtoken <- liftIO $ genAuthToken 128
		storeP2PAuthToken addr authtoken
		return $ P2PAddressAuth addr authtoken

-- Address is read from stdin, to avoid leaking it in shell history.
linkRemote :: RemoteName -> CommandStart
linkRemote remotename = starting "p2p link" ai si $
	next promptaddr
  where
	ai = ActionItemOther (Just (UnquotedString remotename))
	si = SeekInput []
	promptaddr = do
		liftIO $ putStrLn ""
		liftIO $ putStr "Enter peer address: "
		liftIO $ hFlush stdout
		s <- liftIO getLine
		if null s
			then do
				liftIO $ hPutStrLn stderr "Nothing entered, giving up."
				return False
			else case unformatP2PAddress s of
				Nothing -> do
					liftIO $ hPutStrLn stderr "Unable to parse that address, please check its format and try again."
					promptaddr
				Just addr -> do
					r <- setupLink remotename addr
					case r of
						LinkSuccess -> return True
						ConnectionError e -> giveup e
						AuthenticationError e -> giveup e

startPairing :: RemoteName -> [P2PAddress] -> CommandStart
startPairing _ [] = giveup "No P2P networks are currently available."
startPairing remotename addrs = ifM (liftIO Wormhole.isInstalled)
	( starting "p2p pair" ai si $
		performPairing remotename addrs
	, giveup "Magic Wormhole is not installed, and is needed for pairing. Install it from your distribution or from https://github.com/warner/magic-wormhole/"
	)
  where
	ai = ActionItemOther (Just (UnquotedString remotename))
	si = SeekInput []

performPairing :: RemoteName -> [P2PAddress] -> CommandPerform
performPairing remotename addrs = do
	-- This note is displayed mainly so when magic wormhole
	-- complains about possible protocol mismatches or other problems,
	-- it's clear what's doing the complaining.
	showNote "using Magic Wormhole"
	next $ do
		showOutput
		r <- wormholePairing remotename addrs ui
		case r of
			PairSuccess -> return True
			SendFailed -> do
				warning "Failed sending data to pair."
				return False
			ReceiveFailed -> do
				warning "Failed receiving data from pair."
				return False
			LinkFailed e -> do
				warning $ UnquotedString $ "Failed linking to pair: " ++ e
				return False
  where
	ui observer producer = do
		ourcode <- Wormhole.waitCode observer
		putStrLn ""
		putStrLn $ "This repository's pairing code is: " ++
			Wormhole.fromCode ourcode
		putStrLn ""
		theircode <- getcode ourcode
		Wormhole.sendCode producer theircode
	
	getcode ourcode = do
		putStr "Enter the other repository's pairing code: "
		hFlush stdout
		l <- getLine
		case Wormhole.toCode l of
			Just code
				| code /= ourcode -> do
					putStrLn "Exchanging pairing data..."
					return code
				| otherwise -> do
					putStrLn "Oops -- You entered this repository's pairing code. Enter the pairing code of the *other* repository."
					getcode ourcode
			Nothing -> do
				putStrLn "That does not look like a valiad pairing code. Try again..."
				getcode ourcode

-- We generate half of the authtoken; the pair will provide
-- the other half.
newtype HalfAuthToken = HalfAuthToken T.Text
	deriving (Show)

data PairData = PairData HalfAuthToken [P2PAddress]
	deriving (Show)

serializePairData :: PairData -> String
serializePairData (PairData (HalfAuthToken ha) addrs) = unlines $
	T.unpack ha : map formatP2PAddress addrs

deserializePairData :: [String] -> Maybe PairData
deserializePairData [] = Nothing
deserializePairData (ha:l) = do
	addrs <- mapM unformatP2PAddress l
	return (PairData (HalfAuthToken (T.pack ha)) addrs)

data PairingResult
	= PairSuccess
	| SendFailed
	| ReceiveFailed
	| LinkFailed String

wormholePairing
	:: RemoteName
	-> [P2PAddress]
	-> (Wormhole.CodeObserver -> Wormhole.CodeProducer -> IO ())
	-> Annex PairingResult
wormholePairing remotename ouraddrs ui = do
	ourhalf <- liftIO $ HalfAuthToken . fromAuthToken
		<$> genAuthToken 64
	let ourpairdata = PairData ourhalf ouraddrs

	-- The magic wormhole interface only supports exchanging
	-- files. Permissions of received files may allow others
	-- to read them. So, set up a temp directory that only
	-- we can read.
	withTmpDir (literalOsPath "pair") $ \tmp -> do
		liftIO $ void $ tryIO $ modifyFileMode tmp $ 
			removeModes otherGroupModes
		let sendf = tmp </> literalOsPath "send"
		let recvf = tmp </> literalOsPath "recv"
		liftIO $ writeFileProtected sendf $
			serializePairData ourpairdata

		observer <- liftIO Wormhole.mkCodeObserver
		producer <- liftIO Wormhole.mkCodeProducer
		void $ liftIO $ async $ ui observer producer
		-- Provide an appid to magic wormhole, to avoid using
		-- the same channels that other wormhole users use.
		let appid = Wormhole.appId "git-annex.branchable.com/p2p-setup"
		(sendres, recvres) <- liftIO $
			Wormhole.sendFile (fromOsPath sendf) observer appid
				`concurrently`
			Wormhole.receiveFile (fromOsPath recvf) producer appid
		liftIO $ removeWhenExistsWith removeFile sendf
		if sendres /= True
			then return SendFailed
			else if recvres /= True
				then return ReceiveFailed
				else do
					r <- liftIO $ tryIO $
						map decodeBS . fileLines'
							<$> F.readFile' recvf
					case r of
						Left _e -> return ReceiveFailed
						Right ls -> maybe 
							(return ReceiveFailed)
							(finishPairing 100 remotename ourhalf)
							(deserializePairData ls)

-- | Allow the peer we're pairing with to authenticate to us,
-- using an authtoken constructed from the two HalfAuthTokens.
-- Connect to the peer we're pairing with, and try to link to them.
--
-- Multiple addresses may have been received for the peer. This only
-- makes a link to one address.
--
-- Since we're racing the peer as they do the same, the first try is likely
-- to fail to authenticate. Can retry any number of times, to avoid the
-- users needing to redo the whole process.
finishPairing :: Int -> RemoteName -> HalfAuthToken -> PairData -> Annex PairingResult
finishPairing retries remotename (HalfAuthToken ourhalf) (PairData (HalfAuthToken theirhalf) theiraddrs) = do
	case (toAuthToken (ourhalf <> theirhalf), toAuthToken (theirhalf <> ourhalf)) of
		(Just ourauthtoken, Just theirauthtoken) -> do
			liftIO $ putStrLn $ "Successfully exchanged pairing data. Connecting to " ++ remotename ++  "..."
			go retries theiraddrs theirauthtoken ourauthtoken
		_ -> return ReceiveFailed
  where
	go 0 [] _ _ = return $ LinkFailed $ "Unable to connect to " ++ remotename ++ "."
	go n [] theirauthtoken ourauthtoken = do
		liftIO $ threadDelaySeconds (Seconds 2)
		liftIO $ putStrLn $ "Unable to connect to " ++ remotename ++ ". Retrying..."
		go (n-1) theiraddrs theirauthtoken ourauthtoken
	go n (addr:rest) theirauthtoken ourauthtoken = do
		storeP2PAuthToken addr ourauthtoken
		r <- setupLink remotename (P2PAddressAuth addr theirauthtoken)
		case r of
			LinkSuccess -> return PairSuccess
			_ -> go n rest theirauthtoken ourauthtoken

data LinkResult
	= LinkSuccess
	| ConnectionError String
	| AuthenticationError String

setupLink :: RemoteName -> P2PAddressAuth -> Annex LinkResult
setupLink remotename (P2PAddressAuth addr authtoken) = do
	g <- Annex.gitRepo
	cv <- liftIO $ tryNonAsync $ connectPeer (Just g) addr
	case cv of
		Left e -> return $ ConnectionError $ "Unable to connect with peer. Please check that the peer is connected to the network, and try again. ("  ++ show e ++ ")"
		Right conn -> do
			u <- getUUID
			let proto = P2P.auth u authtoken noop
			runst <- liftIO $ mkRunState Client
			go =<< liftIO (runNetProto runst conn proto)
  where
	go (Right (Just theiruuid)) = do
		ok <- inRepo $ Git.Command.runBool
			[ Param "remote", Param "add"
			, Param remotename
			, Param (formatP2PAddress addr)
			]
		when ok $ do
			storeUUIDIn (remoteAnnexConfig remotename "uuid") theiruuid
			storeP2PRemoteAuthToken addr authtoken
		return LinkSuccess
	go (Right Nothing) = return $ AuthenticationError "Unable to authenticate with peer. Please check the address and try again."
	go (Left e) = return $ AuthenticationError $ "Unable to authenticate with peer: " ++ describeProtoFailure e

enableNetwork :: P2PNetName -> CommandStart
enableNetwork netname@(P2PNetName name)
	| name == "tor" = EnableTor.start Nothing
	| otherwise = starting "p2p enable" ai si $ next $ do
		addrs <- liftIO $ getAddressGenericP2P netname
		when (null addrs) $
			giveup $ genericP2PCommand netname ++ " did not output any P2P addresses" 
		mapM_ storeP2PAddress addrs
		return True
  where
	ai = ActionItemOther (Just (UnquotedString name))
	si = SeekInput []
