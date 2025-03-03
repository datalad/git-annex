{- git-annex exports
 -
 - Copyright 2017-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Export where

import Annex
import Annex.CatFile
import Annex.GitShaKey
import Types
import qualified Git
import qualified Types.Remote as Remote
import Git.Quote
import Messages

-- From a sha pointing to the content of a file to the key
-- to use to export it. When the file is annexed, it's the annexed key.
-- When the file is stored in git, it's a special type of key to indicate
-- that.
exportKey :: Git.Sha -> Annex Key
exportKey sha = mk <$> catKey sha
  where
	mk (Just k) = k
	mk Nothing = gitShaKey sha

warnExportImportConflict :: Remote -> Annex ()
warnExportImportConflict r = do
	isimport <- Remote.isImportSupported r
	isexport <- Remote.isExportSupported r
	let (ops, resolvcmd) = case (isexport, isimport) of
		(False, True) -> ("imported from", "git-annex import")
		(True, False) -> ("exported to", "git-annex export")
		_ -> ("exported to and/or imported from", "git-annex export")
	toplevelWarning True $ UnquotedString $ unwords
		[ "Conflict detected. Different trees have been"
		, ops, Remote.name r ++ ". Use"
		, resolvcmd
		, "to resolve this conflict."
		]
