{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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
      User (..)
    , File (..)
    , ST (..)
    , getUser
    , initState
    , initUsers
    , initFiles
    , GetUser (..)
    , AddUser (..)
    , GetFile (..)
    , GetFiles (..)
    , AddFile (..)
) where


import qualified Data.Map as M
import           Data.ByteString (ByteString)
import           Data.Map (Map, fromList)
import           Data.SafeCopy (deriveSafeCopy, base)
import           Data.Acid (AcidState, Update, Query, makeAcidic)
import           Data.Typeable (Typeable)
import           Control.Monad.Reader (ask)
import           Control.Monad.State (get, put)
import           System.IO.Unsafe (unsafePerformIO)
import           Crypto.PasswordStore (makePassword)

data User = User {
      username :: String
    , passHash :: ByteString
} deriving (Show, Typeable)

data Users = Users (Map String User)

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Users)

addUser :: User -> Update Users ()
addUser user = do
    Users users <- get
    put $ Users $ M.insert (username user) user users

getUser :: String -> Query Users (Maybe User)
getUser username = do
    Users users <- ask
    return $ M.lookup username users

$(makeAcidic ''Users ['addUser, 'getUser])

root = User "root" $ unsafePerformIO $ makePassword "rpass" 14

guest = User "guest" $ unsafePerformIO $ makePassword "pass" 14

initUsers = Users $ fromList [("root", root), ("guest", guest)]

data File = File {
      filename :: String
    , fileowner :: User
    , filedata :: String
} deriving (Show, Typeable)

data Files = Files [File]

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
    let filesMap = map (\ f -> (filename f, f)) files
    return $ lookup fname filesMap

$(makeAcidic ''Files ['addFile, 'getFile, 'getFiles])

initFiles = Files []

data ST = ST {
      currUser :: User
    , users :: AcidState Users
    , files :: AcidState Files
}

initState = ST {
      currUser = guest
    , users = error "No db connection."
    , files = error "No db connection."
}
