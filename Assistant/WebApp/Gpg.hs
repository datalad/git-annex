{- git-annex webapp gpg stuff
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Assistant.WebApp.Gpg where

import Assistant.WebApp.Common
import Utility.Gpg
import Utility.UserInfo

import qualified Data.Map as M

gpgKeyDisplay :: KeyId -> Maybe UserId -> Widget
gpgKeyDisplay keyid userid = [whamlet|
  <span title="key id #{keyid}">
    <i .icon-user></i> #
      ^{displayname}
|]
  where
  	displayname = case userid of
		Just name | not (null name) -> [whamlet|#{name}|]
		_ -> [whamlet|key id #{keyid}|]

{- Generates a gpg user id that is not used by any existing secret key -}
newUserId :: IO UserId
newUserId = do
	oldkeys <- secretKeys
	username <- myUserName
  	let basekeyname = username ++ "'s git-annex encryption key"
	return $ Prelude.head $ filter (\n -> M.null $ M.filter (== n) oldkeys)
		( basekeyname
		: map (\n -> basekeyname ++ show n) ([2..] :: [Int])
		)

withNewSecretKey :: (KeyId -> Handler Html) -> Handler Html
withNewSecretKey use = do
	userid <- liftIO $ newUserId
	liftIO $ genSecretKey RSA "" userid maxRecommendedKeySize
	results <- M.keys . M.filter (== userid) <$> liftIO secretKeys
	case results of
		[] -> error "Failed to generate gpg key!"
		(key:_) -> use key
