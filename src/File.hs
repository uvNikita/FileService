{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
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
import User (User, username)
import Data.SafeCopy (deriveSafeCopy, base)

data Permissions = N | R | W | RW deriving (Eq)

instance Show Permissions where
    show N = "--"
    show R = "r-"
    show W = "-w"
    show RW = "rw"

instance Read Permissions where
    readsPrec _ ('-':'-':rest) = [(N, rest)]
    readsPrec _ ('r':'-':rest) = [(R, rest)]
    readsPrec _ ('-':'w':rest) = [(W, rest)]
    readsPrec _ ('r':'w':rest) = [(RW, rest)]
    readsPrec _ _ = []

instance Ord Permissions where
    compare RW RW = EQ
    compare N N = EQ
    compare RW _ = GT
    compare _ N = LT
    compare p1 p2 | p1 == p2 = EQ
                  | otherwise = LT

$(deriveSafeCopy 0 'base ''Permissions)


data File = File {
      filename :: String
    , fileowner :: User
    , filedata :: String
    , fileperms :: Permissions
} deriving (Typeable)

instance Eq File where
    f1 == f2 = filename f1 == filename f2

instance Show File where
    show f = filename f ++ "\t" ++
             show (fileperms f) ++ "\t" ++
             username (fileowner f)

$(deriveSafeCopy 0 'base ''File)
