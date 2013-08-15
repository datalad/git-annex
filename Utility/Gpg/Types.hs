{- gpg data types
 -
 - Copyright 2013 guilhem <guilhem@fripost.org>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Gpg.Types where

import Utility.SafeCommand
import Types.GitConfig
import Types.Remote

{- GnuPG options. -}
type GpgOpt = String
newtype GpgOpts = GpgOpts [GpgOpt]

toParams :: GpgOpts -> [CommandParam]
toParams (GpgOpts opts) = map Param opts

class LensGpgOpts a where
	getGpgOpts :: a -> GpgOpts

{- Extract the GnuPG options from a Remote Git Config. -}
instance LensGpgOpts RemoteGitConfig where
	getGpgOpts = GpgOpts . remoteAnnexGnupgOptions

{- Extract the GnuPG options from a Remote. -}
instance LensGpgOpts (RemoteA a) where
	getGpgOpts = getGpgOpts . gitconfig
