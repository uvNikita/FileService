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
import           Control.Applicative ((<$>))
import           Control.Monad (when)
import           Control.Monad.Trans (lift)
import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State (StateT, runStateT, get, put, liftM, liftIO)
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

type Args = [String]

class Executable a where
    excName :: a -> String
    excArgsNum :: a -> Int
    excUsage :: a -> String
    performAction :: a ->  Args -> Action ()
    exc :: a -> Args -> Action ()
    exc e args = if length args == excArgsNum e
                    then performAction e args
                    else raise $ "Usage: " ++ excUsage e

toMap :: Executable a => [a] -> Map.Map String a
toMap cs = Map.fromList (map (\ c -> (excName c, c)) cs)

data Command = Command {
      comName :: String
    , comAction :: Args -> Action ()
    , comArgsNum :: Int
    , comUsage :: String
}

instance Executable Command where
    excName = comName
    excArgsNum = comArgsNum
    excUsage = comUsage
    performAction = comAction

data FileCommand = FileCommand {
      fcomName :: String
    , fcomAction :: F.File -> Args -> Action ()
    , fcomUsage :: String
    , fcomArgsNum :: Int
    , fcomPerms :: F.Permissions
}

instance Executable FileCommand where
    excName = fcomName
    excArgsNum c = fcomArgsNum c + 1
    excUsage = fcomUsage
    performAction c (filename:args) = do
        filesDB <- liftM DB.files getDB
        file <- io $ query filesDB (DB.GetFile filename)
        currUser <- liftM S.currUser getST
        let canProcess f = hasPerms currUser f (fcomPerms c)
        maybe (raise $ "No such file: " ++ filename)
              (\ f -> if canProcess f
                         then fcomAction c f args
                         else do
                            io $ warningM logger $ "Permission Denied: " ++ excName c
                            raise "Permission Denied.")
              (file)
    performAction c [] = raise $ "Usage: " ++ excUsage c

hasPerms :: U.User -> F.File -> F.Permissions -> Bool
hasPerms user file perms = F.fileowner file == user ||
                           F.fileperms file >= perms ||
                           user == U.root

io :: IO a -> Action a
io = liftIO

commands :: Map String Command
commands = toMap coms
           where coms = [login, printUser, addUser, addFile
                       , listFiles, timeLeft, delUser]
fcommands :: Map String FileCommand
fcommands = toMap fcoms
            where fcoms = [catFile, rmFile, putInFile, chMod]

login = Command {
      comName = "login"
    , comAction = \ [username, password] -> do
          users <- liftM DB.users getDB
          maybeUser <- io $ query users (DB.GetUser username)
          let pass = B.pack password
          let nonValid = do
              raise "Invalid username or password."
              io $  warningM logger "Failed login"
          case maybeUser of
              Nothing -> nonValid
              Just user -> if verifyPassword pass $ U.passHash user
                          then getST >>= (\ st -> put $ st {S.currUser = user})
                          else nonValid
    , comArgsNum = 2
    , comUsage = "login username password"

}

addUser = Command {
      comName = "adduser"
    , comAction = \ [username, password] ->
          if not $ U.validPass password
              then raise "Not valid pass."
              else do
                   users <- liftM DB.users getDB
                   let pass = B.pack password
                   passHash <- io $ makePassword pass 14
                   let user = U.User username passHash
                   dbUser <- io $ query users (DB.GetUser username)
                   let addUser' = io $ update users (DB.AddUser user)
                   maybe addUser' (\ _ -> raise "User exists.") dbUser
    , comArgsNum = 2
    , comUsage = "adduser username password"
}

delUser = Command {
      comName = "deluser"
    , comAction = \ [username] -> do
          currUser <- liftM S.currUser getST
          if currUser /= U.root
             then do
                raise "Only root can delete users."
                io $ warningM logger "Non root tried to delete user."
             else do
                users <- liftM DB.users getDB
                user <- io $query users (DB.GetUser username)
                maybe (raise "No such user")
                      (io . update users . DB.DelUser)
                      (user)
    , comArgsNum = 1
    , comUsage = "deluser username"
}

printUser = Command {
      comName = "pu"
    , comAction = \ [] -> getST >>= (io . putStrLn . U.username . S.currUser)
    , comArgsNum = 0
    , comUsage = "pu"
}

addFile = Command {
      comName = "addfile"
    , comAction = \ [fname, fdata] -> do
          files <- liftM DB.files getDB
          owner <- liftM S.currUser getST
          dbFile <- io $ query files (DB.GetFile fname)
          let file = F.File fname owner fdata F.R
          let addFile' = io $ update files (DB.AddFile file)
          maybe addFile' (\ _ -> raise "File exists.") dbFile
     , comArgsNum = 2
     , comUsage = "addfile filename myfiledata"
}

listFiles = Command {
      comName = "ls"
    , comAction = \ [] -> do
          filesDB <- liftM DB.files getDB
          files <- io $ query filesDB DB.GetFiles
          io $ mapM_ print files
    , comArgsNum = 0
    , comUsage = "ls"
}

catFile = FileCommand {
      fcomName = "cat"
    , fcomAction = \ f [] -> io $ putStrLn $ F.filedata f
    , fcomUsage = "cat filename"
    , fcomPerms = F.R
    , fcomArgsNum = 0
}

putInFile = FileCommand {
      fcomName = "put"
    , fcomAction = \ f [fdata] -> do
        let nf = f {F.filedata = fdata}
        replace f nf
    , fcomUsage = "put filename \"new data\""
    , fcomPerms = F.W
    , fcomArgsNum = 1
}

rmFile = FileCommand {
      fcomName = "rm"
    , fcomAction = \ f [] -> do
        files <- liftM DB.files getDB
        io $ update files (DB.DelFile f)
    , fcomUsage = "rm filename"
    , fcomPerms = F.W
    , fcomArgsNum = 0
}

chMod = FileCommand {
      fcomName = "chmod"
    , fcomAction = \ f [perms] ->
        case maybeRead perms of
            Nothing -> raise "Invalid permissions."
            Just ps -> do
                let nf = f {F.fileperms = ps}
                replace f nf
    , fcomUsage = "chmod filename r-"
    , fcomPerms = F.W
    , fcomArgsNum = 1
}

timeLeft = Command {
      comName = "tl"
    , comAction = \ [] -> do
          currTime <- io timestamp
          valTime <- liftM S.valTime getST
          let tl = 60 - (currTime - valTime)
          io $ print tl
    , comArgsNum = 0
    , comUsage = "tl"
}

replace :: F.File -> F.File -> Action ()
replace file newfile = do
    files <- liftM DB.files getDB
    _ <- io $ update files (DB.DelFile file)
    io $ update files (DB.AddFile newfile)

excCommand :: String -> [String] -> Action ()
excCommand cName args = do
    let noCommand = raise $ "No such command: " ++ cName
    case Map.lookup cName commands of
        Just c -> exc c args
        Nothing -> maybe noCommand (`exc` args) (Map.lookup cName fcommands)

raise :: String -> Action ()
raise msg = io $ putStrLn msg

timestamp :: IO (DiffTime)
timestamp = liftM utctDayTime getCurrentTime

checkAuth :: Action Bool
checkAuth = do
    valTime <- liftM S.valTime getST
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
    put state {S.valTime = currTime}

askQuestion :: Action (Bool)
askQuestion = do
    x1 <- io $ randomRIO (1, 10)
    x2 <- io $ randomRIO (1, 10)
    io $ putStrLn $ show x1 ++ " + " ++ show x2 ++ " = "
    res <- io $ liftM maybeRead getLine :: Action (Maybe Int)
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
