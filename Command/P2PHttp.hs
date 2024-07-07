{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.P2PHttp where

import Command
import P2P.Http

cmd :: Command
cmd = command "p2phttp" SectionPlumbing
	"communicate in P2P protocol over http"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = error "TODO"
