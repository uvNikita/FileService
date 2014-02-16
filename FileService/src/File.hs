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
) where


import Data.Typeable (Typeable)
import User (User)


data File = File {
      filename :: String
    , fileowner :: User
    , filedata :: String
} deriving (Show, Typeable)
