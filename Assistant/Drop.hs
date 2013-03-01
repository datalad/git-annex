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
import Types.Remote (AssociatedFile, uuid)
import qualified Remote
import qualified Command.Drop
import Command
import Annex.Wanted
import Annex.Exception
import Config

import qualified Data.Set as S

type Reason = String

{- Drop from local and/or remote when allowed by the preferred content and
 - numcopies settings. -}
handleDrops :: Reason -> Bool -> Key -> AssociatedFile -> Maybe Remote -> Assistant ()
handleDrops _ _ _ Nothing _ = noop
handleDrops reason fromhere key f knownpresentremote = do
	syncrs <- syncDataRemotes <$> getDaemonStatus
	locs <- liftAnnex $ loggedLocations key
	handleDropsFrom locs syncrs reason fromhere key f knownpresentremote

{- The UUIDs are ones where the content is believed to be present.
 - The Remote list can include other remotes that do not have the content;
 - only ones that match the UUIDs will be dropped from.
 - If allows to drop fromhere, that drop will be tried first. -}
handleDropsFrom :: [UUID] -> [Remote] -> Reason -> Bool -> Key -> AssociatedFile -> Maybe Remote -> Assistant ()
handleDropsFrom _ _ _ _ _ Nothing _ = noop
handleDropsFrom locs rs reason fromhere key (Just f) knownpresentremote
	| fromhere = do
		n <- getcopies
		if checkcopies n
			then go rs =<< dropl n
			else go rs n
	| otherwise = go rs =<< getcopies
  where
	getcopies = liftAnnex $ do
		have <- length <$> trustExclude UnTrusted locs
		numcopies <- getNumCopies =<< numCopies f
		return (have, numcopies)
	checkcopies (have, numcopies) = have > numcopies
	decrcopies (have, numcopies) = (have - 1, numcopies)

	go [] _ = noop
	go (r:rest) n
		| uuid r `S.notMember` slocs = go rest n
		| checkcopies n = dropr r n >>= go rest
		| otherwise = noop

	checkdrop n@(have, numcopies) u a =
		ifM (liftAnnex $ wantDrop True u (Just f))
			( ifM (liftAnnex $ safely $ doCommand $ a (Just numcopies))
				( do
					debug
						[ "dropped"
						, f
						, "(from " ++ maybe "here" show u ++ ")"
						, "(copies now " ++ show (have - 1) ++ ")"
						, ": " ++ reason
						]
					return $ decrcopies n
				, return n
				)
			, return n
			)

	dropl n = checkdrop n Nothing $ \numcopies ->
		Command.Drop.startLocal f numcopies key knownpresentremote

	dropr r n  = checkdrop n (Just $ Remote.uuid r) $ \numcopies ->
		Command.Drop.startRemote f numcopies key r

	safely a = either (const False) id <$> tryAnnex a

	slocs = S.fromList locs
