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
import System.Console.Readline (readline, addHistory)

authLoop = do
    isValid <- checkAuth
    unless isValid $ do
        raise "Not valid user."
        authLoop

loop = do
    authLoop
    maybeLine <- io $ readline "# "
    case maybeLine of
        Nothing -> return ()
        Just "exit" -> return ()
        Just line -> do
            let (cName, args) = parseCommand line
            io $ addHistory line
            excCommand cName args
            loop

main = run loop
