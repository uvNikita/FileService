-----------------------------------------------------------------------------
--
-- Module      :  Console
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

module Console (
      run
    , excCommand
    , readInput
    , raise
    , checkAuth
) where

import qualified File as F
import qualified User as U
import           User (User)
import qualified State as S
import           State (ST)
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Database as DB
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (listToMaybe)
import           Data.Acid (openLocalStateFrom, closeAcidState, query, update)
import           Data.List.Split (splitOn)
import           Data.Time.Clock (DiffTime, getCurrentTime, utctDayTime)
import           Control.Exception (finally)
import           Control.Monad (unless, when)
import           Control.Monad.State (StateT, runStateT, get, put, liftM, liftIO)
import           Crypto.PasswordStore (makePassword, verifyPassword)
import           System.Random (randomRIO)

type Action a = StateT ST IO a
type Args = [String]

data Command = Command {
      comName :: String
    , comAction :: Args -> Action ()
    , argsNum :: Int
    , usage :: String
}

exc :: Command -> Args -> Action ()
exc command args = if length args == argsNum command
                   then (comAction command) args
                   else raise $ "Usage: " ++ usage command

io :: IO a -> Action a
io = liftIO

commands :: Map.Map String Command
commands = Map.fromList $ map (\ c -> (comName c, c)) cs
           where cs = [login, printUser, addUser, addFile, listFiles, timeLeft, cat]

login = Command {
      comName = "login"
    , comAction = \ [username, password] -> do
          users <- liftM S.users get
          user <- io $ query users (DB.GetUser username)
          let pass = B.pack password
          case user of
              Nothing -> raise "Invalid username or password."
              Just user -> if verifyPassword pass $ U.passHash user
                          then get >>= (\ st -> put $ st {S.currUser = user})
                          else raise "Invalid username or password"
    , argsNum = 2
    , usage = "login username password"

}

addUser = Command {
      comName = "adduser"
    , comAction = \ [username, password] -> do
          users <- liftM S.users get
          let pass = B.pack password
          passHash <- io $ makePassword pass 14
          let user = U.User username passHash
          dbUser <- io $ query users (DB.GetUser username)
          let addUser = io $ update users (DB.AddUser user)
          maybe addUser (\ _ -> raise "User exists.") dbUser
    , argsNum = 2
    , usage = "addUser username password"
}

printUser = Command {
      comName = "pu"
    , comAction = \ [] -> get >>= (io . putStrLn . U.username . S.currUser)
    , argsNum = 0
    , usage = "pu"
}

addFile = Command {
      comName = "addfile"
    , comAction = \ [fname, fdata] -> do
          files <- liftM S.files get
          owner <- liftM S.currUser get
          dbFile <- io $ query files (DB.GetFile fname)
          let file = F.File fname owner fdata
          let addFile = io $ update files (DB.AddFile file)
          maybe addFile (\ _ -> raise "File exists.") dbFile
     , argsNum = 2
     , usage = "addfile filename myfiledata"
}

listFiles = Command {
      comName = "ls"
    , comAction = \ [] -> do
          filesDB <- liftM S.files get
          files <- io $ query filesDB DB.GetFiles
          let f2str f = F.filename f ++ " : " ++ (U.username . F.fileowner) f
          io $ mapM_ (putStrLn . f2str) files
    , argsNum = 0
    , usage = "ls"
}

cat = Command {
      comName = "cat"
    , comAction = \ [filename] -> do
        filesDB <- liftM S.files get
        file <- io $ query filesDB (DB.GetFile filename)
        maybe (raise $ "No such file: " ++ filename)
              (\ f -> io $ putStrLn $ F.filedata f)
              file
    , argsNum = 1
    , usage = "cat filename"
}

timeLeft = Command {
      comName = "tl"
    , comAction = \ [] -> do
          currTime <- io timestamp
          valTime <- liftM S.valTime get
          let tl = 60 - (currTime - valTime)
          io $ print tl
    , argsNum = 0
    , usage = "tl"
}

readInput :: Action (String, [String])
readInput = liftM parseLine (io getLine)

parseLine :: String -> (String, [String])
parseLine "" = ("", [""])
parseLine line = (command, args)
                 where command : args = filter (not . null) $ splitOn " " line

excCommand :: String -> [String] -> Action ()
excCommand cName args =
    maybe (raise $ "No such command: " ++ cName) (`exc` args) (getCommand cName)

getCommand :: String -> Maybe Command
getCommand name = Map.lookup name commands

raise msg = io $ putStrLn msg

timestamp = liftM utctDayTime getCurrentTime

checkAuth :: Action Bool
checkAuth = do
    valTime <- liftM S.valTime get
    currTime <- io timestamp
    if currTime - valTime < 60
        then return True
        else do isValid <- askQuestion
                when isValid updateValTime
                return isValid

updateValTime :: Action ()
updateValTime = do
    currTime <- io timestamp
    state <- get
    put state {S.valTime = currTime}

askQuestion = do
    x1 <- io $ randomRIO (1, 10)
    x2 <- io $ randomRIO (1, 10)
    io $ putStrLn $ show x1 ++ " + " ++ show x2 ++ " = "
    res <- io $ liftM maybeRead getLine
    return $ validateCalc x1 x2 res

validateCalc :: Int -> Int -> Maybe Int -> Bool
validateCalc _ _ Nothing = False
validateCalc x1 x2 (Just res) = res == x1 + x2

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

run code = do
    users <- openLocalStateFrom "/tmp/file_service/users" DB.initUsers
    files <- openLocalStateFrom "/tmp/file_service/files" DB.initFiles
    currTime <- timestamp
    let initState = S.ST {
          S.currUser = U.guest
        , S.users = users
        , S.files = files
        , S.valTime = currTime
    }
    finally (runStateT code initState) $ do
        closeAcidState users
        closeAcidState files
