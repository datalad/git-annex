{- git-annex command
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.FilterProcess where

import Command
import qualified Command.Smudge
import Git.FilterProcess
import Git.PktLine
import Annex.Link

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = noCommit $ noMessages $
	command "filter-process" SectionPlumbing 
		"long running git filter process"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek _ = liftIO longRunningFilterProcessHandshake >>= \case
	Left err -> giveup err
	Right () -> go
  where
	go = liftIO getFilterRequest >>= \case
		Just (Smudge f) -> do
			smudge f
			go
		Just (Clean f) -> do
			clean f
			go
		Nothing -> return ()

smudge :: OsPath -> Annex ()
smudge file = do
	{- The whole git file content is necessarily buffered in memory,
	 - because we have to consume everything git is sending before
	 - we can respond to it. An annexed file will be only a pointer
	 - though. -}
	b <- B.concat . map pktLineToByteString <$> liftIO readUntilFlushPkt
	Command.Smudge.smudge' file (L.fromStrict b)
	{- Git expects us to output the content of unlocked annexed files,
	 - but if we got a pointer, we output only the pointer.
	 - See Command.Smudge.smudge for details of how this works. -}
	liftIO $ respondFilterRequest b

clean :: OsPath -> Annex ()
clean file = do
	{- We have to consume everything git is sending before we can
	 - respond to it. But it can be an arbitrarily large file,
	 - which is being added to the annex, and we do not want to buffer
	 - all that in memory. 
	 -
	 - Start by reading enough to determine if the file is an annex
	 - pointer.
	 -}
	let conv b l = (B.concat (map pktLineToByteString l), b)
	(b, readcomplete) <- 
		either (conv False) (conv True)
			<$> liftIO (readUntilFlushPktOrSize maxPointerSz)
	
	let passthrough
		| readcomplete = liftIO $ respondFilterRequest b
		| otherwise = liftIO $ do
			-- Have to buffer the file content in memory here,
			-- but it's not an annexed file, so not typically
			-- large, and it's all stored in git, which also
			-- buffers files in memory.
			b' <- B.concat . (map pktLineToByteString)
				<$> readUntilFlushPkt
			respondFilterRequest (b <> b')
	let discardreststdin
		| readcomplete = return ()
		| otherwise = liftIO discardUntilFlushPkt
	let emitpointer = liftIO . respondFilterRequest . formatPointer
	-- This does not incrementally hash, so both git and git-annex
	-- read from the file. It may be less expensive to incrementally
	-- hash the content provided by git, but Backend does not currently
	-- have an interface to do so.
	Command.Smudge.clean' file
		(parseLinkTargetOrPointer' b)
		passthrough
		discardreststdin
		emitpointer
