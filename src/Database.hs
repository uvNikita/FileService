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
    , DelUser (..)
    , GetFile (..)
    , GetFiles (..)
    , AddFile (..)
    , DelFile (..)
    , DB (..)
    , initFiles
    , initUsers
) where


import qualified File as F
import           File (File)
import qualified User as U
import           User (User)
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.List (find)
import           Data.SafeCopy (deriveSafeCopy, base)
import           Data.Acid (AcidState, Update, Query, makeAcidic)
import           Control.Monad.Reader (ask)
import           Control.Monad.State (get, put)


data Files = Files [File]

data Users = Users (Map String User)

$(deriveSafeCopy 0 'base ''Users)

addUser :: User -> Update Users ()
addUser user = do
    Users users <- get
    put $ Users $ M.insert (U.username user) user users

delUser :: User -> Update Users ()
delUser user = do
    Users users <- get
    let username = U.username user
    put $ Users $ M.delete username users

getUser :: String -> Query Users (Maybe User)
getUser username = do
    Users users <- ask
    return $ M.lookup username users

$(makeAcidic ''Users ['addUser, 'getUser, 'delUser])

type DBUsers = AcidState Users

$(deriveSafeCopy 0 'base ''Files)

addFile :: File -> Update Files ()
addFile file = do
    Files files <- get
    put $ Files $ file : files

delFile :: File -> Update Files ()
delFile file = do
    Files files <- get
    put $ Files $ filter (/= file) files

getFiles :: Query Files [File]
getFiles = do
    Files files <- ask
    return files

getFile :: String -> Query Files (Maybe File)
getFile fname = do
    Files files <- ask
    return $ findFile fname files

findFile :: String -> [File] -> Maybe File
findFile fname = find ((== fname) . F.filename)

$(makeAcidic ''Files ['addFile, 'getFile, 'getFiles, 'delFile])

type DBFiles = AcidState Files

data DB = DB {
      users :: DBUsers
    , files :: DBFiles
}

initFiles = Files []

initUsers = Users $ M.fromList [("root", U.root), ("guest", U.guest)]
