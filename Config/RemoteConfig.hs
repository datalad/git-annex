{- git-annex remote config parsing
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Config.RemoteConfig where

import qualified Data.Map as M
import Data.Typeable

import Types.RemoteConfig
import Types.ProposedAccepted
import Config

parseRemoteConfig :: RemoteConfig -> [RemoteConfigParser] -> Either String ParsedRemoteConfig
parseRemoteConfig c = go [] (M.filterWithKey notaccepted c)
  where
	go l c' []
		| M.null c' = Right (M.fromList l)
		| otherwise = Left $ "Unexpected fields: " ++
			unwords (map fromProposedAccepted (M.keys c'))
	go l c' ((f, p):rest) = do
		v <- p (M.lookup f c) c
		go ((f,v):l) (M.delete f c') rest
	notaccepted (Proposed _) _ = True
	notaccepted (Accepted _) _ = False

yesNoParser :: RemoteConfigField -> Bool -> RemoteConfigParser
yesNoParser f fallback = (f, p)
  where
	p v _c = case v of
		Nothing -> Right (RemoteConfigValue fallback)
		Just v' -> case yesNo (fromProposedAccepted v') of
			Just b -> Right (RemoteConfigValue b)
			Nothing -> case v' of
				Accepted _ -> Right (RemoteConfigValue fallback)
				Proposed _ -> Left $
					"bad " ++ fromProposedAccepted f ++
					" value (expected yes or no)"

optStringParser :: RemoteConfigField -> RemoteConfigParser
optStringParser f = (f, \v _c -> Right (RemoteConfigValue v))
