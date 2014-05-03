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
            io $ addHistory line
            case parseCommand line of
                Left err -> raise $ show err
                Right (comName, args) -> excCommand comName args
            loop

main :: IO ()
main = run loop
