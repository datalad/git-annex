{- git-annex command data types
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Command where

import Data.Ord
import Options.Applicative.Types (Parser)

import Types

{- A command runs in these stages.
 -
 - a. The parser stage parses the command line and generates a CommandSeek
 -    action. -}
type CommandParser = Parser CommandSeek
{- b. The check stage runs checks, that error out if
 -    anything prevents the command from running. -}
data CommandCheck = CommandCheck { idCheck :: Int, runCheck :: Annex () }
{- c. The seek stage is passed input from the parser, looks through
 -    the repo to find things to act on (ie, new files to add), and
 -    runs commandAction to handle all necessary actions. -}
type CommandSeek = Annex ()
{- d. The start stage is run before anything is printed about the
 -    command, is passed some input, and can early abort it
 -    if the input does not make sense. It should run quickly and
 -    should not modify Annex state. -}
type CommandStart = Annex (Maybe CommandPerform)
{- e. The perform stage is run after a message is printed about the command
 -    being run, and it should be where the bulk of the work happens. -}
type CommandPerform = Annex (Maybe CommandCleanup)
{- f. The cleanup stage is run only if the perform stage succeeds, and it
 -    returns the overall success/fail of the command. -}
type CommandCleanup = Annex Bool

{- A command is defined by specifying these things. -}
data Command = Command
	{ cmdcheck :: [CommandCheck] -- check stage
	, cmdnocommit :: Bool        -- don't commit journalled state changes
	, cmdnomessages :: Bool      -- don't output normal messages
	, cmdname :: String
	, cmdparamdesc :: CmdParamsDesc -- description of params for usage
	, cmdsection :: CommandSection
	, cmddesc :: String          -- description of command for usage
	, cmdparser :: CommandParser -- command line parser
	, cmdnorepo :: Maybe (Parser (IO ())) -- used when not in a repo
	}

{- Command-line parameters, after the command is selected and options
 - are parsed. -}
type CmdParams = [String]

type CmdParamsDesc = String

{- CommandCheck functions can be compared using their unique id. -}
instance Eq CommandCheck where
	a == b = idCheck a == idCheck b

instance Eq Command where
	a == b = cmdname a == cmdname b

{- Order commands by name. -}
instance Ord Command where
	compare = comparing cmdname

{- The same sections are listed in doc/git-annex.mdwn -}
data CommandSection 
	= SectionCommon
	| SectionSetup
	| SectionMaintenance
	| SectionQuery
	| SectionMetaData
	| SectionUtility
	| SectionPlumbing
	| SectionTesting
	deriving (Eq, Ord, Enum, Bounded)

descSection :: CommandSection -> String
descSection SectionCommon = "Commonly used commands"
descSection SectionSetup = "Repository setup commands"
descSection SectionMaintenance = "Repository maintenance commands"
descSection SectionQuery = "Query commands"
descSection SectionMetaData = "Metadata commands"
descSection SectionUtility = "Utility commands"
descSection SectionPlumbing = "Plumbing commands"
descSection SectionTesting = "Testing commands"
