{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.SafeCopy (deriveSafeCopy, base)


data User = User {
      username :: String
    , passHash :: ByteString
} deriving (Show, Typeable)

instance Eq User where
    User n1 _ == User n2 _ = n1 == n2

$(deriveSafeCopy 0 'base ''User)

validPass :: String -> Bool
validPass pass = length pass > 3

root :: User
root = User "root" $ unsafePerformIO $ makePassword "rpass" 14

guest :: User
guest = User "guest" $ unsafePerformIO $ makePassword "pass" 14
