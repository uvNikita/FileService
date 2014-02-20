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
import Util (parseCommand)

authLoop :: Action ()
authLoop = do
    isValid <- checkAuth
    unless isValid $ do
        raise "Not valid user."
        authLoop

loop :: Action ()
loop = do
    authLoop
    maybeLine <- io $ readline "# "
    case maybeLine of
        Nothing -> io $ putStrLn ""
        Just "exit" -> return ()
        Just line -> do
            let (cName, args) = parseCommand line
            io $ addHistory line
            excCommand cName args
            loop

main :: IO ()
main = run loop
