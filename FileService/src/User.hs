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
) where

import Data.Map (Map, fromList)
import Data.Typeable (Typeable)
import Data.ByteString (ByteString)
import System.IO.Unsafe (unsafePerformIO)
import Crypto.PasswordStore (makePassword)


data User = User {
      username :: String
    , passHash :: ByteString
} deriving (Show, Typeable)

root = User "root" $ unsafePerformIO $ makePassword "rpass" 14

guest = User "guest" $ unsafePerformIO $ makePassword "pass" 14
