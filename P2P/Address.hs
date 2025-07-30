{- P2P protocol addresses
 -
 - Copyright 2016-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module P2P.Address where

import qualified Annex
import Annex.Common
import Git
import Git.Types
import Creds
import Utility.AuthToken
import Utility.Tor
import qualified Utility.RawFilePath as R

import qualified Data.Text as T
import System.PosixCompat.Files (fileOwner, fileGroup)

-- | A P2P address, without an AuthToken.
--
-- This is enough information to connect to the peer,
-- but not enough to authenticate with it.
data P2PAddress
	= TorAnnex OnionAddress OnionPort
	| P2PAnnex P2PNetName UnderlyingP2PAddress
	deriving (Eq, Show)

newtype P2PNetName = P2PNetName String
	deriving (Eq, Show)

newtype UnderlyingP2PAddress = UnderlyingP2PAddress String
	deriving (Eq, Show)

-- | A P2P address, with an AuthToken.
--
-- This is enough information to connect to the peer, and authenticate with
-- it.
data P2PAddressAuth = P2PAddressAuth P2PAddress AuthToken
	deriving (Eq, Show)

class FormatP2PAddress a where
	formatP2PAddress :: a -> String
	unformatP2PAddress :: String -> Maybe a

instance FormatP2PAddress P2PAddress where
	formatP2PAddress (TorAnnex (OnionAddress onionaddr) onionport) =
		torAnnexScheme ++ ":" ++ onionaddr ++ ":" ++ show onionport
	formatP2PAddress (P2PAnnex (P2PNetName netname) (UnderlyingP2PAddress address)) =
		p2pAnnexScheme ++ ":" ++ netname ++ ":" ++ address
	unformatP2PAddress s
		| schemeprefixed torAnnexScheme = do
			onionport <- readish bs
			return (TorAnnex (OnionAddress as) onionport)
		| schemeprefixed p2pAnnexScheme =
			return (P2PAnnex (P2PNetName as) (UnderlyingP2PAddress bs))
		| otherwise = Nothing
	  where
		schemeprefixed scheme = (scheme ++ ":") `isPrefixOf` s
		(as, bs) = separate (== ':') $
			dropWhile (== ':') $ dropWhile (/= ':') s

torAnnexScheme :: String
torAnnexScheme = "tor-annex:"

p2pAnnexScheme :: String
p2pAnnexScheme = "p2p-annex:"

instance FormatP2PAddress P2PAddressAuth where
	formatP2PAddress (P2PAddressAuth addr authtoken) =
		formatP2PAddress addr ++ ":" ++ T.unpack (fromAuthToken authtoken)
	unformatP2PAddress s = do
		let (ra, rs) = separate (== ':') (reverse s)
		addr <- unformatP2PAddress (reverse rs)
		authtoken <- toAuthToken (T.pack $ reverse ra)
		return (P2PAddressAuth addr authtoken)

repoP2PAddress :: Repo -> Maybe P2PAddress
repoP2PAddress (Repo { location = Url url }) = unformatP2PAddress (show url)
repoP2PAddress _ = Nothing

-- | Load known P2P addresses for this repository.
loadP2PAddresses :: Annex [P2PAddress]
loadP2PAddresses = mapMaybe unformatP2PAddress . maybe [] lines
	<$> readCreds p2pAddressCredsFile

-- | Store a new P2P address for this repository.
storeP2PAddress :: P2PAddress -> Annex ()
storeP2PAddress addr = do
	addrs <- loadP2PAddresses
	unless (addr `elem` addrs) $ do
		let s = unlines $ map formatP2PAddress (addr:addrs)
		let tmpnam = p2pAddressCredsFile <> literalOsPath ".new"
		writeCreds s tmpnam
		tmpf <- credsFile tmpnam
		destf <- credsFile p2pAddressCredsFile
		-- This may be run by root, so make the creds file
		-- and directory have the same owner and group as
		-- the git repository directory has.
		st <- liftIO . R.getFileStatus . fromOsPath
			=<< Annex.fromRepo repoPath
		let fixowner f = R.setOwnerAndGroup (fromOsPath f) (fileOwner st) (fileGroup st)
		liftIO $ do
			fixowner tmpf
			fixowner (takeDirectory tmpf)
			fixowner (takeDirectory (takeDirectory tmpf))
			renameFile tmpf destf

p2pAddressCredsFile :: OsPath
p2pAddressCredsFile = literalOsPath "p2paddrs"

torAppName :: AppName
torAppName = "tor-annex"
