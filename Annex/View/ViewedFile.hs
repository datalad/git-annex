{- filenames (not paths) used in views
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.View.ViewedFile where

import Common.Annex
import Types.View
import Types.MetaData
import qualified Git
import qualified Git.DiffTree as DiffTree
import qualified Git.Branch
import qualified Git.LsFiles
import qualified Git.Ref
import Git.UpdateIndex
import Git.Sha
import Git.HashObject
import Git.Types
import Git.FilePath
import qualified Backend
import Annex.Index
import Annex.Link
import Annex.CatFile
import Logs.MetaData
import Logs.View
import Utility.Glob
import Utility.FileMode
import Types.Command
import Config
import CmdLine.Action

type FileName = String
type ViewedFile = FileName

type MkViewedFile = FilePath -> ViewedFile

{- Converts a filepath used in a reference branch to the
 - filename that will be used in the view.
 -
 - No two filepaths from the same branch should yeild the same result,
 - so all directory structure needs to be included in the output file
 - in some way. However, the branch's directory structure is not replicated
 - in the view.
 -
 - So, from dir/subdir/file.foo, generate file_{dir;subdir}.foo
 -
 - (To avoid collisions with a filename that already contains {foo},
 - that is doubled to {{foo}}.)
 -}
viewedFileFromReference :: MkViewedFile
viewedFileFromReference f = concat
	[ double base
	, if null dirs then "" else "_{" ++ double (intercalate ";" dirs) ++ "}"
	, double $ concat extensions
	]
  where
	(path, basefile) = splitFileName f
	dirs = filter (/= ".") $ map dropTrailingPathSeparator (splitPath path)
	(base, extensions) = splitShortExtensions basefile

	double = replace "{" "{{" . replace "}" "}}"

viewedFileReuse :: MkViewedFile
viewedFileReuse = takeFileName
