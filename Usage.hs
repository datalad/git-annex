{- git-annex usage messages
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Usage where

import Common.Annex

import Types.Command

import System.Console.GetOpt

usageMessage :: String -> String
usageMessage s = "Usage: " ++ s

{- Usage message with lists of commands by section. -}
usage :: String -> [Command] -> String
usage header cmds = unlines $ usageMessage header : concatMap go [minBound..]
  where
	go section
		| null cs = []
		| otherwise =
			[ ""
			, descSection section ++ ":"
			, ""
			] ++ map cmdline cs
	  where
		cs = filter (\c -> cmdsection c == section) scmds
	cmdline c = concat
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

{- Usage message for a single command. -}
commandUsage :: Command -> String
commandUsage cmd = unlines
	[ usageInfo header (cmdoptions cmd)
	, "To see additional options common to all commands, run: git annex help options"
	]
  where
	header = usageMessage $ unwords
		[ "git-annex"
		, cmdname cmd
		, cmdparamdesc cmd
		, "[option ...]"
		]

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
paramTime :: String
paramTime = "TIME"
paramFormat :: String
paramFormat = "FORMAT"
paramFile :: String
paramFile = "FILE"
paramGroup :: String
paramGroup = "GROUP"
paramExpression :: String
paramExpression = "EXPR"
paramSize :: String
paramSize = "SIZE"
paramAddress :: String
paramAddress = "ADDRESS"
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
