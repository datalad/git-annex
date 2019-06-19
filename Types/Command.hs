{- git-annex command data types
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Command where

import Data.Ord
import Options.Applicative.Types (Parser)

import Types
import Types.DeferredParse
import Types.ActionItem

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
{- d. The start stage is run before anything is output, is passed some
 -    value from the seek stage, and can check if anything needs to be
 -    done, and early abort if not. It should run quickly and should
 -    not modify Annex state or output anything. -}
type CommandStart = Annex (Maybe (StartMessage, CommandPerform))
{- e. The perform stage is run after a message is printed about the command
 -    being run, and it should be where the bulk of the work happens. -}
type CommandPerform = Annex (Maybe CommandCleanup)
{- f. The cleanup stage is run only if the perform stage succeeds, and it
 -    returns the overall success/fail of the command. -}
type CommandCleanup = Annex Bool

{- Message that is displayed when starting to perform an action on
 - something. The String is typically the name of the command or action
 - being performed.
 -}
data StartMessage
	= StartMessage String ActionItem
	| StartUsualMessages String ActionItem
	-- ^ Like StartMessage, but makes sure to enable usual message
	-- display in case it was disabled by cmdnomessages.
	| StartNoMessage ActionItem
	-- ^ Starts, without displaying any message but also without
	-- disabling display of any of the usual messages.
	| CustomOutput ActionItem
	-- ^ Prevents any start, end, or other usual messages from
	-- being displayed, letting a command output its own custom format.
	deriving (Show)

instance MkActionItem StartMessage where
	mkActionItem (StartMessage _ ai) = ai
	mkActionItem (StartUsualMessages _ ai) = ai
	mkActionItem (StartNoMessage ai) = ai
	mkActionItem (CustomOutput ai) = ai

{- A command is defined by specifying these things. -}
data Command = Command
	{ cmdcheck :: [CommandCheck]
	-- ^ check stage
	, cmdnocommit :: Bool
	-- ^ don't commit journalled state changes
	, cmdnomessages :: Bool
	-- ^ don't output normal messages
	, cmdname :: String
	, cmdparamdesc :: CmdParamsDesc
	-- ^ description of params for usage
	, cmdsection :: CommandSection
	, cmddesc :: String
	-- ^ description of command for usage
	, cmdparser :: CommandParser
	-- ^ command line parser
	, cmdglobaloptions :: [GlobalOption]
	-- ^ additional global options
	, cmdnorepo :: Maybe (Parser (IO ()))
	-- ^used when not in a repo
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
