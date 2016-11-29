{- P2P protocol addresses
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module P2P.Address where

import qualified Annex
import Annex.Common
import Git
import Creds
import Utility.AuthToken
import Utility.Tor

import qualified Data.Text as T

-- | A P2P address, without an AuthToken.
--
-- This is enough information to connect to the peer,
-- but not enough to authenticate with it.
data P2PAddress = TorAnnex OnionAddress OnionPort
	deriving (Eq, Show)

-- | A P2P address, with an AuthToken
data P2PAddressAuth = P2PAddressAuth P2PAddress AuthToken
	deriving (Eq, Show)

class FormatP2PAddress a where
	formatP2PAddress :: a -> String
	unformatP2PAddress :: String -> Maybe a

instance FormatP2PAddress P2PAddress where
	formatP2PAddress (TorAnnex (OnionAddress onionaddr) onionport) =
		"tor-annex::" ++ onionaddr ++ ":" ++ show onionport
	unformatP2PAddress s
		| "tor-annex::" `isPrefixOf` s = do
			let s' = dropWhile (== ':') $ dropWhile (/= ':') s
			let (onionaddr, ps) = separate (== ':') s'
			onionport <- readish ps
			return (TorAnnex (OnionAddress onionaddr) onionport)
		| otherwise = Nothing

instance FormatP2PAddress P2PAddressAuth where
	formatP2PAddress (P2PAddressAuth addr authtoken) =
		formatP2PAddress addr ++ ":" ++ T.unpack (fromAuthToken authtoken)
	unformatP2PAddress s = do
		let (ra, rs) = separate (== ':') (reverse s)
		addr <- unformatP2PAddress (reverse rs)
		authtoken <- toAuthToken (T.pack $ reverse ra)
		return (P2PAddressAuth addr authtoken)

-- | Load known P2P addresses for this repository.
loadP2PAddresses :: Annex [P2PAddress]
loadP2PAddresses = mapMaybe unformatP2PAddress . maybe [] lines
	<$> readCacheCreds p2pAddressCredsFile

-- | Store a new P2P address for this repository.
storeP2PAddress :: P2PAddress -> Annex ()
storeP2PAddress addr = do
	addrs <- loadP2PAddresses
	unless (addr `elem` addrs) $ do
		let s = unlines $ map formatP2PAddress (addr:addrs)
		let tmpnam = p2pAddressCredsFile ++ ".new"
		writeCacheCreds s tmpnam
		tmpf <- cacheCredsFile tmpnam
		destf <- cacheCredsFile p2pAddressCredsFile
		-- This may be run by root, so make the creds file
		-- and directory have the same owner and group as
		-- the git repository directory has.
		st <- liftIO . getFileStatus =<< Annex.fromRepo repoLocation
		let fixowner f = setOwnerAndGroup f (fileOwner st) (fileGroup st)
		liftIO $ do
			fixowner tmpf
			fixowner (takeDirectory tmpf)
			fixowner (takeDirectory (takeDirectory tmpf))
			renameFile tmpf destf

p2pAddressCredsFile :: FilePath
p2pAddressCredsFile = "p2paddrs"
