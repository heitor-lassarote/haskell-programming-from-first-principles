{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad (forever)
import qualified Data.ByteString as BS
import           Data.List (intersperse)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import           Text.RawString.QQ

import Fingerd

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users (
    id            INTEGER PRIMARY KEY AUTOINCREMENT,
    username      TEXT    UNIQUE,
    shell         TEXT,
    homeDirectory TEXT,
    realName      TEXT,
    phone         TEXT
)
|]

allUsers :: Query
allUsers = "SELECT * from users"

createDatabase :: IO ()
createDatabase = do
    conn <- open "finger.db"
    execute_ conn createUsers
    execute conn insertUser meRow
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn
  where
    meRow :: UserRow
    meRow = ( Null
            , "heitor"
            , "/bin/zsh"
            , "/home/heitor"
            , "Heitor Toledo Lassarote de Paula"
            , "(01) 9 2345 6789"
            )

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
    rows <- query_ dbConn allUsers
    let usernames = map username rows
        newlineSeparated = T.concat $ intersperse "\n" usernames
    sendAll soc $ encodeUtf8 newlineSeparated

formatUser :: User -> BS.ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
    [ "Login: ",     e username, "\t\t\t\t"
    , "Name: ",      e realName, "\n"
    , "Directory: ", e homeDir,  "\t\t\t"
    , "Shell: ",     e shell, "\n"
    ]
  where
    e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
    maybeUser <- getUser dbConn $ T.strip username
    case maybeUser of
        Nothing ->
            -- TODO: Send this back to client:
            putStrLn $ "Couldn't find matching user for username: " ++ show username
        Just user -> sendAll soc $ formatUser user

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
    msg <- recv soc 1024
    case msg of
        "\r\n" -> returnUsers dbConn soc
        name   -> returnUser dbConn soc $ decodeUtf8 name

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
