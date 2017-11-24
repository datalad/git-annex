{- ssh hostname sanitization
 -
 - When constructing a ssh command with a hostname that may be controlled
 - by an attacker, prevent the hostname from starting with "-",
 - to prevent tricking ssh into arbitrary command execution via
 - eg "-oProxyCommand="
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.SshHost (SshHost, mkSshHost, fromSshHost) where

newtype SshHost = SshHost String

-- | Smart constructor for a legal hostname or IP address.
-- In some cases, it may be prefixed with "user@" to specify the remote
-- user at the host.
--
-- For now, we only filter out the problem ones, because determining an
-- actually legal hostnames is quite complicated.
mkSshHost :: String -> Either String SshHost
mkSshHost h@('-':_) = Left $
	"rejecting ssh hostname that starts with '-' : " ++ h
mkSshHost h = Right (SshHost h)

fromSshHost :: SshHost -> String
fromSshHost (SshHost h) = h
