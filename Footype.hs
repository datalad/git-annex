{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts #-}

module Footype where

import Database.Persist hiding (Key)
import Database.Persist.TH
import Database.Persist.Sqlite hiding (Key)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Time.Clock

import Types.Key
import Types.UUID
import Types.MetaData

-- has to be in a separate file from foo.hs for silly reasons
derivePersistField "Key"
derivePersistField "UUID"
derivePersistField "MetaField"
