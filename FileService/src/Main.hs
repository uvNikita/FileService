-----------------------------------------------------------------------------
--
-- Module      :  Main
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
import Console
import Control.Monad (unless)

authLoop = do
    isValid <- checkAuth
    unless isValid $ do
        raise "Not valid user."
        authLoop

loop = do
    authLoop
    (cName, args) <- readInput
    unless (cName == "exit") $ do
        excCommand cName args
        loop

main = run loop
