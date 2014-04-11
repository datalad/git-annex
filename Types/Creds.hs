{- credentials
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Creds where

type Creds = String -- can be any data that contains credentials

type CredPair = (String, String) -- login, password
