{- git-annex assistant dropping of unwanted content
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Drop where

import Assistant.Common
import Assistant.DaemonStatus
import Logs.Location
import Logs.Trust
import Types.Remote (AssociatedFile)
import qualified Remote
import qualified Command.Drop
import Command
import Annex.Wanted
import Config

{- Drop from syncable remotes when allowed by the preferred content and
 - numcopies settings. -}
handleRemoteDrops :: DaemonStatusHandle -> Key -> AssociatedFile -> Annex ()
handleRemoteDrops dstatus key (Just f) = do
	syncrs <- liftIO $ syncRemotes <$> getDaemonStatus dstatus
	locs <- loggedLocations key
	handleDrops locs syncrs False f key
handleRemoteDrops _ _ _ = noop

{- Drop from local and/or remote when allowed by the preferred content and
 - numcopies settings. -}
handleDrops :: [UUID] -> [Remote] -> Bool -> FilePath -> Key -> Annex ()
handleDrops locs rs fromhere f key
	| fromhere = do
		n <- getcopies
		if checkcopies n
			then go rs =<< dropl n
			else go rs n
	| otherwise = go rs =<< getcopies
	where
		getcopies = do
			have <- length . snd <$> trustPartition UnTrusted locs
			numcopies <- getNumCopies =<< numCopies f
			return (have, numcopies)
		checkcopies (have, numcopies) = have > numcopies
		decrcopies (have, numcopies) = (have - 1, numcopies)

		go [] _ = noop
		go (r:rest) n
			| checkcopies n = dropr r n >>= go rest
			| otherwise = noop

		checkdrop n@(_, numcopies) u a = 
			ifM (wantDrop u (Just f))
				( ifM (doCommand $ a (Just numcopies))
					( return $ decrcopies n
					, return n
					)
				, return n
				)

		dropl n = checkdrop n Nothing $ \numcopies ->
			Command.Drop.startLocal f numcopies key

		dropr r n  = checkdrop n (Just $ Remote.uuid r) $ \numcopies ->
			Command.Drop.startRemote f numcopies key r
