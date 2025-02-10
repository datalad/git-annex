{- git-annex command
 -
 - Copyright 2012-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.AddUnused where

import Logs.Location
import Command
import Annex.Ingest
import Command.Unused (withUnusedMaps, UnusedMaps(..), startUnused)

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "addunused" SectionMaintenance 
		"add back unused files"
		(paramRepeating paramNumRange) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withUnusedMaps start

start :: UnusedMaps -> Int -> CommandStart
start = startUnused go (other "bad") (other "tmp")
  where
	go n key = do
		let file = literalOsPath "unused." <> keyFile key
		starting "addunused"
			(ActionItemTreeFile file)
			(SeekInput [show n]) $
			next $ do
				logStatus NoLiveUpdate key InfoPresent
				addSymlink file key Nothing
				return True

	{- The content is not in the annex, but in another directory, and
	 - it seems better to error out, rather than moving bad/tmp content
	 - into the annex. -}
	other n _ _ = giveup $ "cannot addunused " ++ n ++ "content"

