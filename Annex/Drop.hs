{- dropping of unwanted content
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Drop where

import Annex.Common
import Logs.Trust
import Annex.NumCopies
import Types.Remote (uuid, appendonly, config, remotetype, thirdPartyPopulated)
import qualified Remote
import qualified Command.Drop
import Command
import Annex.Wanted
import Annex.Content
import Annex.SpecialRemote.Config
import qualified Database.Keys

import qualified Data.Set as S

type Reason = String

{- Drop a key from local and/or remote when allowed by the preferred content,
 - required content, and numcopies settings.
 -
 - Skips trying to drop from remotes that are appendonly, since those drops
 - would presumably fail. Also skips dropping from exporttree/importtree
 - remotes, which don't allow dropping individual keys, and from
 - thirdPartyPopulated remotes.
 -
 - The UUIDs are ones where the content is believed to be present.
 - The Remote list can include other remotes that do not have the content;
 - only ones that match the UUIDs will be dropped from.
 -
 - If allowed to drop fromhere, that drop will be done last. This is done
 - because local drops do not need any LockedCopy evidence, and so dropping
 - from local last allows the content to be removed from more remotes.
 -
 - A VerifiedCopy can be provided as an optimisation when eg, a key
 - has just been uploaded to a remote.
 -
 - The runner is used to run CommandStart sequentially, it's typically 
 - callCommandAction.
 -}
handleDropsFrom :: [UUID] -> [Remote] -> Reason -> Bool -> Key -> AssociatedFile -> SeekInput -> [VerifiedCopy] -> (CommandStart -> CommandCleanup) -> Annex ()
handleDropsFrom locs rs reason fromhere key afile si preverified runner = do
	fs <- Database.Keys.getAssociatedFilesIncluding afile key
	n <- getcopies fs
	void $ if fromhere && checkcopies n Nothing
		then go fs rs n >>= dropl fs
		else go fs rs n
  where
	getcopies fs = do
		(untrusted, have) <- trustPartition UnTrusted locs
		(numcopies, mincopies) <- getSafestNumMinCopies' afile key fs
		return (numCopiesCount have, numcopies, mincopies, S.fromList untrusted)

	{- Check that we have enough copies still to drop the content.
	 - When the remote being dropped from is untrusted, it was not
	 - counted as a copy, so having only numcopies suffices. Otherwise,
	 - we need more than numcopies to safely drop.
	 -
	 - This is not the final check that it's safe to drop, but it
	 - avoids doing extra work to do that check later in cases where it
	 - will surely fail.
	 -}
	checkcopies (have, numcopies, mincopies, _untrusted) Nothing =
		have > fromNumCopies numcopies && have > fromMinCopies mincopies
	checkcopies (have, numcopies, mincopies, untrusted) (Just u)
		| S.member u untrusted = have >= fromNumCopies numcopies && have >= fromMinCopies mincopies
		| otherwise = have > fromNumCopies numcopies && have > fromMinCopies mincopies
	
	decrcopies (have, numcopies, mincopies, untrusted) Nothing =
		(have - 1, numcopies, mincopies, untrusted)
	decrcopies v@(_have, _numcopies, _mincopies, untrusted) (Just u)
		| S.member u untrusted = v
		| otherwise = decrcopies v Nothing

	go _ [] n = pure n
	go fs (r:rest) n
		| uuid r `S.notMember` slocs = go fs rest n
		| appendonly r = go fs rest n
		| exportTree (config r) = go fs rest n
		| importTree (config r) = go fs rest n
		| thirdPartyPopulated (remotetype r) = go fs rest n
		| checkcopies n (Just $ Remote.uuid r) =
			dropr fs r n >>= go fs rest
		| otherwise = pure n

	checkdrop fs n u a = do
		let afs = map (AssociatedFile . Just) fs
		let pcc = Command.Drop.PreferredContentChecked True
		lu <- prepareLiveUpdate u key RemovingKey
		ifM (wantDrop lu True u (Just key) afile (Just afs))
			( dodrop n u (a lu pcc)
			, return n
			)

	dodrop n@(have, numcopies, mincopies, _untrusted) u a = 
		ifM (safely $ runner $ a numcopies mincopies)
			( do
				fastDebug "Annex.Drop" $ unwords
					[ "dropped"
					, case afile of
						AssociatedFile Nothing -> serializeKey key
						AssociatedFile (Just af) -> fromRawFilePath af
					, "(from " ++ maybe "here" show u ++ ")"
					, "(copies now " ++ show (have - 1) ++ ")"
					, ": " ++ reason
					]
				return $ decrcopies n u
			, return n
			)

	dropl fs n = checkdrop fs n Nothing $ \lu pcc numcopies mincopies ->
		stopUnless (inAnnex key) $
			Command.Drop.startLocal lu pcc afile ai si 
				numcopies mincopies key preverified
				(Command.Drop.DroppingUnused False)

	dropr fs r n  = checkdrop fs n (Just $ Remote.uuid r) $ \lu pcc numcopies mincopies ->
		Command.Drop.startRemote lu pcc afile ai si 
			numcopies mincopies key
			(Command.Drop.DroppingUnused False) r

	ai = mkActionItem (key, afile)

	slocs = S.fromList locs
	
	safely a = either (const False) id <$> tryNonAsync a

