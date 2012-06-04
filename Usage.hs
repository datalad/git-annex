{- git-annex usage messages
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Usage where

import Common.Annex
import System.Console.GetOpt

import Types.Command

{- Usage message with lists of commands and options. -}
usage :: String -> [Command] -> [Option] -> String
usage header cmds commonoptions = unlines $ 
	[ header
	, ""
	, "Options:"
	] ++ optlines ++
	[ ""
	, "Commands:"
	, ""
	] ++ cmdlines
	where
		-- To get consistent indentation of options, generate the
		-- usage for all options at once. A command's options will
		-- be displayed after the command.
		alloptlines = filter (not . null) $
			lines $ usageInfo "" $
				concatMap cmdoptions scmds ++ commonoptions
		(cmdlines, optlines) = go scmds alloptlines []
		go [] os ls = (ls, os)
		go (c:cs) os ls = go cs os' (ls++(l:o))
			where
				(o, os') = splitAt (length $ cmdoptions c) os
				l = concat 
					[ cmdname c
					, namepad (cmdname c)
					, cmdparamdesc c
					, descpad (cmdparamdesc c)
					, cmddesc c
					]
		pad n s = replicate (n - length s) ' '
		namepad = pad $ longest cmdname + 1
		descpad = pad $ longest cmdparamdesc + 2
		longest f = foldl max 0 $ map (length . f) cmds
		scmds = sort cmds

{- Descriptions of params used in usage messages. -}
paramPaths :: String
paramPaths = paramOptional $ paramRepeating paramPath -- most often used
paramPath :: String
paramPath = "PATH"
paramKey :: String
paramKey = "KEY"
paramDesc :: String
paramDesc = "DESC"
paramUrl :: String
paramUrl = "URL"
paramNumber :: String
paramNumber = "NUMBER"
paramNumRange :: String
paramNumRange = "NUM|RANGE"
paramRemote :: String
paramRemote = "REMOTE"
paramGlob :: String
paramGlob = "GLOB"
paramName :: String
paramName = "NAME"
paramValue :: String
paramValue = "VALUE"
paramUUID :: String
paramUUID = "UUID"
paramType :: String
paramType = "TYPE"
paramDate :: String
paramDate = "DATE"
paramFormat :: String
paramFormat = "FORMAT"
paramFile :: String
paramFile = "FILE"
paramKeyValue :: String
paramKeyValue = "K=V"
paramNothing :: String
paramNothing = ""
paramRepeating :: String -> String
paramRepeating s = s ++ " ..."
paramOptional :: String -> String
paramOptional s = "[" ++ s ++ "]"
paramPair :: String -> String -> String
paramPair a b = a ++ " " ++ b
