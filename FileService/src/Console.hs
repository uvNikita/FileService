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


import qualified Data.Map as Map
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

type Action a = StateT DB.ST IO a
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
           where cs = [login, printUser, addUser, addFile, listFiles, timeLeft]

login = Command {
      comName = "login"
    , comAction = \ [username, password] -> do
          users <- liftM DB.users get
          user <- io $ query users (DB.GetUser username)
          let pass = B.pack password
          case user of
              Nothing -> raise "Invalid username or password."
              Just user -> if verifyPassword pass $ DB.passHash user
                          then get >>= (\ st -> put $ st {DB.currUser = user})
                          else raise "Invalid username or password"
    , argsNum = 2
    , usage = "login username password"

}

addUser = Command {
      comName = "adduser"
    , comAction = \ [username, password] -> do
          users <- liftM DB.users get
          let pass = B.pack password
          passHash <- io $ makePassword pass 14
          let user = DB.User username passHash
          dbUser <- io $ query users (DB.GetUser username)
          let addUser = io $ update users (DB.AddUser user)
          maybe addUser (\ _ -> raise "User exists.") dbUser
    , argsNum = 2
    , usage = "addUser username password"
}

printUser = Command {
      comName = "pu"
    , comAction = \ [] -> get >>= (io . putStrLn . DB.username . DB.currUser)
    , argsNum = 0
    , usage = "pu"
}

addFile = Command {
      comName = "addfile"
    , comAction = \ [fname, fdata] -> do
          files <- liftM DB.files get
          owner <- liftM DB.currUser get
          dbFile <- io $ query files (DB.GetFile fname)
          let file = DB.File fname owner fdata
          let addFile = io $ update files (DB.AddFile file)
          maybe addFile (\ _ -> raise "File exists.") dbFile
     , argsNum = 2
     , usage = "addfile filename myfiledata"
}

listFiles = Command {
      comName = "ls"
    , comAction = \ [] -> do
          filesDB <- liftM DB.files get
          files <- io $ query filesDB DB.GetFiles
          let f2str f = DB.filename f ++ " : " ++ (DB.username . DB.fileowner) f
          io $ mapM_ (putStrLn . f2str) files
    , argsNum = 0
    , usage = "ls"
}

timeLeft = Command {
      comName = "tl"
    , comAction = \ [] -> do
          currTime <- io timestamp
          valTime <- liftM DB.valTime get
          let tl = 60 - (currTime - valTime)
          io $ putStrLn $ show tl
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
    valTime <- liftM DB.valTime get
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
    put state {DB.valTime = currTime}

askQuestion = do
    x1 <- io $ randomRIO (1, 10)
    x2 <- io $ randomRIO (1, 10)
    io $ putStrLn $ (show x1) ++ " + " ++ (show x2) ++ " = "
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
    let initState = DB.ST {
          DB.currUser = DB.guest
        , DB.users = users
        , DB.files = files
        , DB.valTime = currTime
    }
    finally (runStateT code initState) $ do
        closeAcidState users
        closeAcidState files
