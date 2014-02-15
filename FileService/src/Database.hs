{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
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
      User
    , ST (..)
    , getUser
    , userName
    , initState
    , initUsers
    , GetUser (..)
) where


import qualified Data.Map as M
import           Data.Map (Map, fromList)
import           Data.SafeCopy (deriveSafeCopy, base)
import           Data.Acid (AcidState, Update, Query, openLocalStateFrom, makeAcidic)
import           Data.Typeable (Typeable)
import           Control.Monad.Reader (ask)
import           Control.Monad.State (get, put)

data User = User {
    userName :: String
} deriving (Show, Typeable)

data Users = Users (Map String User)

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''Users)

data ST = ST {
      currUser :: User
    , users :: AcidState Users
}

addUser :: User -> Update Users ()
addUser user
    = do Users users <- get
         put $ Users $ M.insert (userName user) user users

getUser :: String -> Query Users (Maybe User)
getUser username
    = do Users users <- ask
         return $ M.lookup username users

$(makeAcidic ''Users ['addUser, 'getUser])

root = User "root"

initUsers = Users $ fromList [("root", root)]

initState = ST {
      users = error "No db connection."
    , currUser = root
}
