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
    , parseCommand
    , io
    , raise
    , checkAuth
) where

import qualified File as F
import qualified User as U
import qualified State as S
import           Util
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Database as DB
import qualified Data.ByteString.Char8 as B
import           Data.Acid (openLocalStateFrom, closeAcidState, query, update)
import           Data.List.Split (splitOn)
import           Data.Time.Clock (DiffTime, getCurrentTime, utctDayTime)
import           Control.Exception (finally)
import           Control.Monad (unless, when, void)
import           Control.Monad.State (StateT, runStateT, get, put, liftM, liftIO)
import           Crypto.PasswordStore (makePassword, verifyPassword)
import           System.IO (Handle)
import           System.Random (randomRIO)
import           System.Log.Handler.Simple (fileHandler, GenericHandler)
import           System.Log.Handler (setFormatter, close)
import           System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger,
                                    Priority(INFO), Priority(WARNING),
                                    infoM, warningM, setLevel)
import           System.Log.Formatter (simpleLogFormatter)

logger = rootLoggerName

type Action a = StateT S.ST IO a
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
        filesDB <- liftM S.files get
        file <- io $ query filesDB (DB.GetFile filename)
        currUser <- liftM S.currUser get
        let canProcess f = hasPerms currUser f (fcomPerms c)
        maybe (raise $ "No such file: " ++ filename)
              (\ f -> if canProcess f
                         then fcomAction c f args
                         else raise "Permission Denied.")
              (file)

hasPerms :: U.User -> F.File -> F.Permissions -> Bool
hasPerms user file perms = F.fileowner file == user ||
                           F.fileperms file >= perms ||
                           user == U.root

io :: IO a -> Action a
io = liftIO

(commands, fcommands) = (toMap coms, toMap fcoms)
                        where coms = [login, printUser, addUser, addFile
                                    , listFiles, timeLeft, delUser]
                              fcoms = [catFile, rmFile, putInFile, chMod]

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
    , comArgsNum = 2
    , comUsage = "login username password"

}

addUser = Command {
      comName = "adduser"
    , comAction = \ [username, password] ->
          if not $ U.validPass password
              then void $ raise "Not valid pass."
              else do
                   users <- liftM S.users get
                   let pass = B.pack password
                   passHash <- io $ makePassword pass 14
                   let user = U.User username passHash
                   dbUser <- io $ query users (DB.GetUser username)
                   let addUser = io $ update users (DB.AddUser user)
                   maybe addUser (\ _ -> raise "User exists.") dbUser
    , comArgsNum = 2
    , comUsage = "adduser username password"
}

delUser = Command {
      comName = "deluser"
    , comAction = \ [username] -> do
          currUser <- liftM S.currUser get
          if currUser /= U.root
             then raise "Only root can delete users."
             else do
                  users <- liftM S.users get
                  user <- io $query users (DB.GetUser username)
                  maybe (raise "No such user")
                        (io . update users . DB.DelUser)
                        (user)
    , comArgsNum = 1
    , comUsage = "deluser username"
}

printUser = Command {
      comName = "pu"
    , comAction = \ [] -> get >>= (io . putStrLn . U.username . S.currUser)
    , comArgsNum = 0
    , comUsage = "pu"
}

addFile = Command {
      comName = "addfile"
    , comAction = \ [fname, fdata] -> do
          files <- liftM S.files get
          owner <- liftM S.currUser get
          dbFile <- io $ query files (DB.GetFile fname)
          let file = F.File fname owner fdata F.R
          let addFile = io $ update files (DB.AddFile file)
          maybe addFile (\ _ -> raise "File exists.") dbFile
     , comArgsNum = 2
     , comUsage = "addfile filename myfiledata"
}

listFiles = Command {
      comName = "ls"
    , comAction = \ [] -> do
          filesDB <- liftM S.files get
          files <- io $ query filesDB DB.GetFiles
          let f2str f = F.filename f ++ "\t" ++
                        show (F.fileperms f) ++ "\t" ++
                        U.username (F.fileowner f)

          io $ mapM_ (putStrLn . f2str) files
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
        files <- liftM S.files get
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
          valTime <- liftM S.valTime get
          let tl = 60 - (currTime - valTime)
          io $ print tl
    , comArgsNum = 0
    , comUsage = "tl"
}

replace file newfile = do
    files <- liftM S.files get
    io $ update files (DB.DelFile file)
    io $ update files (DB.AddFile newfile)

parseCommand :: String -> (String, [String])
parseCommand "" = ("", [""])
parseCommand line =
    (command, args)
    where (command, rawArgs) = break (== ' ') line
          args = filter (not . null) $ parse rawArgs
          parse "" = [""]
          parse (' ' : cs) = [] : parse cs
          parse ('"' : cs) = arg : parse cs'
                               where (arg, cs') = case break (== '"') cs of
                                                      (arg, '"' : cs'') -> (arg, cs'')
                                                      (arg, cs'') -> (arg, cs'')
          parse (c : rest) = (c : arg) : args
                             where arg : args = parse rest

excCommand :: String -> [String] -> Action ()
excCommand cName args = do
    let noCommand = raise $ "No such command: " ++ cName
    case Map.lookup cName commands of
        Just c -> exc c args
        Nothing -> maybe noCommand (`exc` args) (Map.lookup cName fcommands)

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

setupLogging = do
    let logPath = "/tmp/file_service.log"
    logFileHandler <- fileHandler logPath WARNING
    let logFileHandler' = withFormatter logFileHandler
    updateGlobalLogger logger (setHandlers [logFileHandler'])
    return logFileHandler

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

run code = do
    logHandler <- setupLogging
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
        close logHandler
