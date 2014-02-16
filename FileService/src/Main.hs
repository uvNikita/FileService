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

loop = do
    (cName, args) <- readInput
    unless (cName == "exit") $
        do excCommand cName args
           loop

code = do
    connectDB
    loop
    disconnectDB

main = run code
