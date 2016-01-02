-- | Maintainer: Jelmer Vernooij <jelmer@jelmer.uk>
--
module Propellor.Property.Munin (
	hostListFragment,
	hostListFragment',
	nodePort,
	nodeInstalled,
	nodeRestarted,
	nodeConfPath,
	masterInstalled,
	masterRestarted,
	masterConfPath,
) where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

nodePort :: Integer
nodePort = 4949

nodeInstalled :: Property NoInfo
nodeInstalled = Apt.serviceInstalledRunning "munin-node"

nodeRestarted :: Property NoInfo
nodeRestarted = Service.restarted "munin-node"

nodeConfPath :: FilePath
nodeConfPath = "/etc/munin/munin-node.conf"

masterInstalled :: Property NoInfo
masterInstalled = Apt.serviceInstalledRunning "munin"

masterRestarted :: Property NoInfo
masterRestarted = Service.restarted "munin"

masterConfPath :: FilePath
masterConfPath = "/etc/munin/munin.conf"


-- | Create the host list fragment for master config.
-- Takes an optional override list for hosts that are accessible on a non-standard host/port.
-- TODO(jelmer): Only do this on hosts where munin is present (in other words, with Munin.installedNode)
hostListFragment' :: [Host] -> [(HostName, (IPAddr, Port))] -> [String]
hostListFragment' hs os = concatMap muninHost hs
  where
	muninHost :: Host -> [String]
	muninHost h = [ "[" ++ (hostName h) ++ "]"
		      , "  address " ++ maybe (hostName h) (fromIPAddr . fst) (hOverride h)
		      ] ++ (maybe [] (\x -> ["  port " ++ (show $ fromPort $ snd x)]) (hOverride h)) ++ [""]
	hOverride :: Host -> Maybe (IPAddr, Port)
	hOverride h = lookup (hostName h) os
	fromPort (Port p) = p

-- | Create the host list fragment for master config.
hostListFragment :: [Host] -> [String]
hostListFragment hs = hostListFragment' hs []
