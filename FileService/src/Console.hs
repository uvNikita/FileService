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
import           Data.List.Split (splitOn)
import           Data.Acid (openLocalStateFrom, closeAcidState, query, update)
import           Control.Monad.State (StateT, runStateT, get, put, liftM, liftIO)


type Action a = StateT DB.ST IO a
type Command = [String] -> Action ()

io :: IO a -> Action a
io = liftIO

commands :: Map.Map String Command
commands = Map.fromList [
      ("login", login)
    , ("pu", printUser)
    , ("adduser", addUser)
    ]

login :: Command
login [username] = do
    users <- liftM DB.users get
    user <- io $ query users (DB.GetUser username)
    case user of
        Nothing -> excFail ["Invalid user: " ++ username]
        Just user -> get >>= (\ st -> put $ st {DB.currUser = user})

addUser :: Command
addUser [username] = do
    users <- liftM DB.users get
    let user = DB.User username
    io $ update users (DB.AddUser user)

printUser :: Command
printUser [] = get >>= (io . putStrLn . DB.username . DB.currUser)

readInput :: Action (String, [String])
readInput = io getLine >>= return . parseLine

parseLine :: String -> (String, [String])
parseLine line = (command, args)
                 where command : args = filter (not . null) $ splitOn " " line

excCommand :: String -> [String] -> Action ()
excCommand cName args =
    maybe (excFail ["No such command: " ++ cName]) (\ com -> com args) (getCommand cName)

getCommand :: String -> Maybe Command
getCommand name = Map.lookup name commands

excFail :: Command
excFail [msg] = io $ putStrLn msg

connectDB :: Action ()
connectDB = do
    connectUsers "/tmp/file_service/users"

disconnectDB :: Action ()
disconnectDB = do
    disconnectUsers

disconnectUsers :: Action ()
disconnectUsers = do
    users <- liftM DB.users get
    io $ closeAcidState users

connectUsers :: String -> Action ()
connectUsers path = do
    users <- io $ openLocalStateFrom path DB.initUsers
    state <- get
    put $ state {DB.users = users}

run code = do
    runStateT code DB.initState
    return ()
