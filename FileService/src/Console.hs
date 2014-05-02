{-# LANGUAGE NamedFieldPuns #-}
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
    , io
    , raise
    , checkAuth
    , Action
) where

import qualified File as F
import qualified User as U
import qualified State as S
import           Util
import qualified Database as DB
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.ByteString.Char8 as B
import           Data.Acid (openLocalStateFrom, closeAcidState, query, update)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (DiffTime, getCurrentTime, utctDayTime)
import           Control.Exception (finally)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (when)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State (StateT, runStateT, get, put, liftIO)
import           Crypto.PasswordStore (makePassword, verifyPassword)
import           System.IO (Handle)
import           System.Random (randomRIO)
import           System.Log.Handler.Simple (fileHandler, GenericHandler)
import           System.Log.Handler (setFormatter, close)
import           System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger,
                                    Priority(WARNING), warningM)
import           System.Log.Formatter (simpleLogFormatter)

logger :: String
logger = rootLoggerName

type Action = ReaderT DB.DB (StateT S.ST IO)

getDB :: Action DB.DB
getDB = ask

getST :: Action S.ST
getST = lift get

putST :: S.ST -> Action ()
putST = lift . put

type Args = [String]

toMap :: [Command] -> Map.Map String Command
toMap = Map.fromList . map ((,) <$> (name . info) <*> id)

data CommandInfo = CommandInfo {
      name :: String
    , argsNum :: Int
    , usage :: String
}

data Command = Command { info :: CommandInfo
                       , action :: Args -> Action () }
             | FileCommand { info :: CommandInfo
                           , fileAction :: F.File -> Args -> Action ()
                           , permissions :: F.Permissions}

showUsage :: Command -> Action ()
showUsage = raise . ("Usage: " ++) . usage . info

exc :: Command -> Args ->  Action ()
exc com@(Command {}) args
    | length args < argsNum (info com) = showUsage com
    | otherwise = action com args

exc com@(FileCommand {}) [] = showUsage com
exc com@(FileCommand {fileAction, permissions}) (filename:args)
    | length args + 1 < argsNum (info com) = showUsage com
    | otherwise = do
        filesDB <- DB.files <$> getDB
        file <- io $ query filesDB (DB.GetFile filename)
        currUser <- S.currUser <$> getST
        let canProcess f = hasPerms currUser f permissions
        maybe (raise $ "No such file: " ++ filename)
              (\ f -> if canProcess f
                         then fileAction f args
                         else do
                            io $ warningM logger $ "Permission Denied: " ++ name (info com)
                            raise "Permission Denied.")
              (file)

hasPerms :: U.User -> F.File -> F.Permissions -> Bool
hasPerms user file perms = F.fileowner file == user ||
                           F.fileperms file >= perms ||
                           user == U.root

io :: IO a -> Action a
io = liftIO

commands :: Map String Command
commands = toMap coms
           where coms = [login, printUser, addUser, addFile
                       , listFiles, timeLeft, delUser, catFile
                       , rmFile, putInFile, chMod]
login :: Command
login = Command {
      info = CommandInfo { name = "login"
                         , argsNum = 2
                         , usage = "login username password" }
    , action = \ [username, password] -> do
          users <- DB.users <$> getDB
          maybeUser <- io $ query users (DB.GetUser username)
          let pass = B.pack password
          let nonValid = do
              raise "Invalid username or password."
              io $  warningM logger "Failed login"
          case maybeUser of
              Nothing -> nonValid
              Just user -> if verifyPassword pass $ U.passHash user
                          then getST >>= (\ st -> putST $ st {S.currUser = user})
                          else nonValid
}

addUser :: Command
addUser = Command {
      info = CommandInfo { name = "adduser"
                         , argsNum = 2
                         , usage = "adduser username password" }
    , action = \ [username, password] ->
          if not $ U.validPass password
              then raise "Not valid pass."
              else do
                   users <- DB.users <$> getDB
                   let pass = B.pack password
                   passHash <- io $ makePassword pass 14
                   let user = U.User username passHash
                   dbUser <- io $ query users (DB.GetUser username)
                   let addUser' = io $ update users (DB.AddUser user)
                   maybe addUser' (\ _ -> raise "User exists.") dbUser
}

delUser :: Command
delUser = Command {
      info = CommandInfo { name = "deluser"
                         , argsNum = 1
                         , usage = "deluser username" }
    , action = \ [username] -> do
          currUser <- S.currUser <$> getST
          if currUser /= U.root
             then do
                raise "Only root can delete users."
                io $ warningM logger "Non root tried to delete user."
             else do
                users <- DB.users <$> getDB
                user <- io $query users (DB.GetUser username)
                maybe (raise "No such user")
                      (io . update users . DB.DelUser)
                      (user)
}

printUser :: Command
printUser = Command {
      info = CommandInfo { name = "pu"
                         , argsNum = 0
                         , usage = "pu" }
    , action = \ [] -> getST >>= (io . putStrLn . U.username . S.currUser)
}

addFile :: Command
addFile = Command {
      info = CommandInfo { name = "addfile"
                         , argsNum = 2
                         , usage = "addfile filename myfiledata" }
    , action = \ [fname, fdata] -> do
          files <- DB.files <$> getDB
          owner <- S.currUser <$> getST
          dbFile <- io $ query files (DB.GetFile fname)
          let file = F.File fname owner fdata F.R
          let addFile' = io $ update files (DB.AddFile file)
          maybe addFile' (\ _ -> raise "File exists.") dbFile
}

listFiles :: Command
listFiles = Command {
      info = CommandInfo { name = "ls"
                         , argsNum = 0
                         , usage = "ls" }
    , action = \ [] -> do
          filesDB <- DB.files <$> getDB
          files <- io $ query filesDB DB.GetFiles
          io $ mapM_ print files
}

catFile :: Command
catFile = FileCommand {
      info = CommandInfo { name = "cat"
                         , argsNum = 1
                         , usage = "cat filename" }
    , fileAction = \ f [] -> io $ putStrLn $ F.filedata f
    , permissions = F.R
}

putInFile :: Command
putInFile = FileCommand {
      info = CommandInfo { name = "put"
                         , argsNum = 2
                         , usage = "put filename \"new data\"" }
    , fileAction = \ f [fdata] -> do
        let nf = f {F.filedata = fdata}
        replace f nf
    , permissions = F.W
}

rmFile :: Command
rmFile = FileCommand {
      info = CommandInfo { name = "rm"
                         , argsNum = 1
                         , usage = "rm filename" }
    , fileAction = \ f [] -> do
        files <- DB.files <$> getDB
        io $ update files (DB.DelFile f)
    , permissions = F.W
}

chMod :: Command
chMod = FileCommand {
      info = CommandInfo { name = "chmod"
                         , argsNum = 2
                         , usage = "chmod filename r-" }
    , fileAction = \ f [perms] ->
        case maybeRead perms of
            Nothing -> raise "Invalid permissions."
            Just ps -> do
                let nf = f {F.fileperms = ps}
                replace f nf
    , permissions = F.W
}

timeLeft :: Command
timeLeft = Command {
      info = CommandInfo { name = "tl"
                         , argsNum = 0
                         , usage = "rl" }
    , action = \ [] -> do
          currTime <- io timestamp
          valTime <- S.valTime <$> getST
          let tl = 60 - (currTime - valTime)
          io $ print tl
}

replace :: F.File -> F.File -> Action ()
replace file newfile = do
    files <- DB.files <$> getDB
    _ <- io $ update files (DB.DelFile file)
    io $ update files (DB.AddFile newfile)

excCommand :: String -> [String] -> Action ()
excCommand cName args =
    case Map.lookup cName commands of
        Just c -> exc c args
        Nothing -> raise $ "No such command: " ++ cName

raise :: String -> Action ()
raise msg = io $ putStrLn msg

timestamp :: IO DiffTime
timestamp = utctDayTime <$> getCurrentTime

checkAuth :: Action Bool
checkAuth = do
    valTime <- S.valTime <$> getST
    currTime <- io timestamp
    if currTime - valTime < 60
        then return True
        else do isValid <- askQuestion
                when isValid updateValTime
                return isValid

updateValTime :: Action ()
updateValTime = do
    currTime <- io timestamp
    state <- getST
    putST state {S.valTime = currTime}

askQuestion :: Action Bool
askQuestion = do
    x1 <- io $ randomRIO (1, 10)
    x2 <- io $ randomRIO (1, 10)
    io $ putStrLn $ show x1 ++ " + " ++ show x2 ++ " = "
    res <- io $ maybeRead <$> getLine :: Action (Maybe Int)
    let maybeValid = (x1 + x2 ==) <$> res
    return $ fromMaybe False maybeValid

setupLogging :: IO (GenericHandler Handle)
setupLogging = do
    let logPath = "/tmp/file_service.log"
    logFileHandler <- fileHandler logPath WARNING
    let logFileHandler' = withFormatter logFileHandler
    updateGlobalLogger logger (setHandlers [logFileHandler'])
    return logFileHandler

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

run :: Action () -> IO ()
run code = do
    logHandler <- setupLogging
    users <- openLocalStateFrom "/tmp/file_service/users" DB.initUsers
    files <- openLocalStateFrom "/tmp/file_service/files" DB.initFiles
    currTime <- timestamp
    let initState = S.ST {
          S.currUser = U.guest
        , S.valTime = currTime
    }
    let db = DB.DB {
          DB.users = users
        , DB.files = files
    }
    _ <- finally (runStateT (runReaderT code db) initState) $ do
        closeAcidState users
        closeAcidState files
        close logHandler
    return ()
