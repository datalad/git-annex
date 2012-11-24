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

{- Drop from local and/or remote when allowed by the preferred content and
 - numcopies settings. If it's known to be present on a particular remote,
 -  -}
handleDrops :: Bool -> Key -> AssociatedFile -> Maybe Remote -> Assistant ()
handleDrops _ _ Nothing _ = noop
handleDrops fromhere key f knownpresentremote = do
	syncrs <- syncDataRemotes <$> getDaemonStatus
	liftAnnex $ do
		locs <- loggedLocations key
		handleDrops' locs syncrs fromhere key f knownpresentremote

handleDrops' :: [UUID] -> [Remote] -> Bool -> Key -> AssociatedFile -> Maybe Remote -> Annex ()
handleDrops' _ _ _ _ Nothing _ = noop
handleDrops' locs rs fromhere key (Just f) knownpresentremote
	| fromhere = do
		n <- getcopies
		if checkcopies n
			then go rs =<< dropl n
			else go rs n
	| otherwise = go rs =<< getcopies
  where
	getcopies = do
		have <- length <$> trustExclude UnTrusted locs
		numcopies <- getNumCopies =<< numCopies f
		return (have, numcopies)
	checkcopies (have, numcopies) = have > numcopies
	decrcopies (have, numcopies) = (have - 1, numcopies)

	go [] _ = noop
	go (r:rest) n
		| checkcopies n = dropr r n >>= go rest
		| otherwise = noop

	checkdrop n@(_, numcopies) u a = ifM (wantDrop u (Just f))
		( ifM (doCommand $ a (Just numcopies))
			( return $ decrcopies n
			, return n
			)
		, return n
		)

	dropl n = checkdrop n Nothing $ \numcopies ->
		Command.Drop.startLocal f numcopies key knownpresentremote

	dropr r n  = checkdrop n (Just $ Remote.uuid r) $ \numcopies ->
		Command.Drop.startRemote f numcopies key r
