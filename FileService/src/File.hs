{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  File
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

module File (
      File (..)
    , Permissions (..)
) where


import Data.Typeable (Typeable)
import User (User)


data File = File {
      filename :: String
    , fileowner :: User
    , filedata :: String
    , fileperms :: Permissions
} deriving (Show, Typeable)

data Permissions = N | R | RW deriving (Show, Eq, Ord)
