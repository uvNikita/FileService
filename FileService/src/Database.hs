{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Database
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Database (
      DBUsers
    , DBFiles
    , GetUser (..)
    , AddUser (..)
    , GetFile (..)
    , GetFiles (..)
    , AddFile (..)
    , initFiles
    , initUsers
) where


import qualified File as F
import           File (File)
import qualified User as U
import           User (User)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.SafeCopy (deriveSafeCopy, base)
import           Data.Acid (AcidState, Update, Query, makeAcidic)
import           Control.Monad.Reader (ask)
import           Control.Monad.State (get, put)


data Files = Files [File]

data Users = Users (Map String User)

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Users)

addUser :: User -> Update Users ()
addUser user = do
    Users users <- get
    put $ Users $ M.insert (U.username user) user users

getUser :: String -> Query Users (Maybe User)
getUser username = do
    Users users <- ask
    return $ M.lookup username users

$(makeAcidic ''Users ['addUser, 'getUser])

type DBUsers = AcidState Users

$(deriveSafeCopy 0 'base ''File)
$(deriveSafeCopy 0 'base ''Files)

addFile :: File -> Update Files ()
addFile file = do
    Files files <- get
    put $ Files $ file : files

getFiles :: Query Files [File]
getFiles = do
    Files files <- ask
    return files

getFile :: String -> Query Files (Maybe File)
getFile fname = do
    Files files <- ask
    let filesMap = map (\ f -> (F.filename f, f)) files
    return $ lookup fname filesMap

$(makeAcidic ''Files ['addFile, 'getFile, 'getFiles])

type DBFiles = AcidState Files

initFiles = Files []

initUsers = Users $ M.fromList [("root", U.root), ("guest", U.guest)]
