{-# LANGUAGE OverloadedStrings #-}

module Fingerd where

import           Control.Exception
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple hiding (bind)
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)
import           Text.RawString.QQ

data User = User
    { userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone)
      = toRow (id_, username, shell, homeDir, realName, phone)

type UserRow = (Null, Text, Text, Text, Text, Text)

data DuplicateData = DuplicateData deriving (Eq, Show)

instance Exception DuplicateData

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
    results <- query conn getUserQuery (Only username)
    case results of
        []     -> return Nothing
        [user] -> return $ Just user
        _      -> throwIO DuplicateData

setupServer :: IO Socket
setupServer = do
    addrinfos <-
        getAddrInfo
            (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
            Nothing
            (Just "79")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    return sock
