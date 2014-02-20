{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  User
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

module User (
      User (..)
    , root
    , guest
    , validPass
) where

import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import System.IO.Unsafe (unsafePerformIO)
import Crypto.PasswordStore (makePassword)


data User = User {
      username :: String
    , passHash :: ByteString
} deriving (Show, Typeable)

instance Eq User where
    User n1 _ == User n2 _ = n1 == n2

validPass :: String -> Bool
validPass pass = length pass > 3

root :: User
root = User "root" $ unsafePerformIO $ makePassword "rpass" 14

guest :: User
guest = User "guest" $ unsafePerformIO $ makePassword "pass" 14
