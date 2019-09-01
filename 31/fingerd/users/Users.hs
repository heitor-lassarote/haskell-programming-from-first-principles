{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (forever)
import qualified Data.ByteString as BS
import           Data.Char (toUpper)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple.Types
import qualified Data.Text as T
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import           Text.RawString.QQ

import Fingerd

editUserQuery :: Query
editUserQuery =
    "UPDATE users SET username = ?, shell = ?, homeDirectory = ?, realName = ?, phone = ? WHERE id = ?"

putRead :: Socket -> BS.ByteString -> IO T.Text
putRead soc str = sendAll soc str >> recv soc 1024 >>= return . T.strip . decodeUtf8

readUserData :: Socket -> IO UserRow
readUserData soc = do
    username <- putRead soc "Username: "
    shell    <- putRead soc "Shell: "
    homeDir  <- putRead soc "Home directory: "
    realName <- putRead soc "Real name: "
    phone    <- putRead soc "Phone: "
    return (Null, username, shell, homeDir, realName, phone)

insertNewUser :: Connection -> Socket -> IO ()
insertNewUser conn soc = readUserData soc >>= execute conn insertUser

editUser :: Connection -> Socket -> IO ()
editUser conn soc = do
    userToEdit <- putRead soc "User to edit: "
    userToEdit' <- getUser conn userToEdit
    case userToEdit' of
        Just userToEdit'' -> do
            (_, u, s, h, r, p) <- readUserData soc
            execute conn editUserQuery (u, s, h, r, p, userId userToEdit'')
        Nothing -> sendAll soc "User not found!"

handleQuery :: Connection -> Socket -> IO ()
handleQuery conn soc = do
    sendAll soc "[A]dd or [E]dit user? "
    op <- (T.unpack . T.strip . decodeUtf8) <$> recv soc 1024
    case toUpper <$> op of
        a | a `elem` ["A", "ADD"]  -> insertNewUser conn soc
        e | e `elem` ["E", "EDIT"] -> editUser conn soc
        _                          -> sendAll soc "Invalid command."

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery dbConn soc
    close soc

main :: IO ()
main = withSocketsDo $ do
    sock <- setupServer

    -- Only one connection open at a time:
    conn <- open "finger.db"
    handleQueries conn sock
    SQLite.close conn
    close sock
