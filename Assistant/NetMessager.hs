{- git-annex assistant out of band network messager interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.NetMessager where

import Assistant.Common
import Assistant.Types.NetMessager

import Control.Concurrent.STM
import Control.Concurrent.MSampleVar

sendNetMessage :: NetMessage -> Assistant ()
sendNetMessage m = 
	(atomically . flip writeTChan m) <<~ (netMessages . netMessager)

waitNetMessage :: Assistant (NetMessage)
waitNetMessage = (atomically . readTChan) <<~ (netMessages . netMessager)

notifyNetMessagerRestart :: Assistant ()
notifyNetMessagerRestart =
	flip writeSV () <<~ (netMessagerRestart . netMessager)

waitNetMessagerRestart :: Assistant ()
waitNetMessagerRestart = readSV <<~ (netMessagerRestart . netMessager)
