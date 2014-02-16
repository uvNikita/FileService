-----------------------------------------------------------------------------
--
-- Module      :  State
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

module State (
    ST (..)
) where


import User (User)
import Database (DBUsers, DBFiles)
import Data.Time.Clock (DiffTime)


data ST = ST {
      currUser :: User
    , users :: DBUsers
    , files :: DBFiles
    , valTime :: DiffTime
}
