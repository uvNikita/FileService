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

loop = do
    (cName, args) <- readInput
    if cName == "exit"
        then return ()
        else do excCommand cName args
                loop

code = do
    connectDB
    loop
    disconnectDB

main = run code
