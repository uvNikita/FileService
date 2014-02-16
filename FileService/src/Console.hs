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
    , connectDB
    , disconnectDB
) where


import qualified Data.Map as Map
import qualified Database as DB
import qualified Data.ByteString.Char8 as B
import           Data.List.Split (splitOn)
import           Data.Acid (openLocalStateFrom, closeAcidState, query, update)
import           Control.Monad.State (StateT, runStateT, get, put, liftM, liftIO)
import           Crypto.PasswordStore (makePassword, verifyPassword)


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
           where cs = [login, printUser, addUser, addFile, listFiles]

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

connectDB :: Action ()
connectDB = do
    connectUsers "/tmp/file_service/users"
    connectFiles "/tmp/file_service/files"

disconnectDB :: Action ()
disconnectDB = do
    let disconnect db = io $ closeAcidState db
    files <- liftM DB.files get
    users <- liftM DB.users get
    disconnect files
    disconnect users

connectUsers :: String -> Action ()
connectUsers path = do
    users <- io $ openLocalStateFrom path DB.initUsers
    state <- get
    put $ state {DB.users = users}

connectFiles :: String -> Action ()
connectFiles path = do
    files <- io $ openLocalStateFrom path DB.initFiles
    state <- get
    put $ state {DB.files = files}

run code = do
    runStateT (connectDB >> code >> disconnectDB) DB.initState
    return ()
