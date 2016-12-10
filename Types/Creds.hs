{- credentials
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Creds where

type Creds = String -- can be any data that contains credentials

type CredPair = (Login, Password)
type Login = String
type Password = String
