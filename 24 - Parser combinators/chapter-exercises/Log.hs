{-# LANGUAGE QuasiQuotes #-}

module Log
    ( Year, Month, Day, Hour, Minute
    , Date (..), Time (..), Activity (..), Entry (..), Log (..)
    , addTime, subTime
    , example, eol, comment, comments, skipToToken
    , parseDate, parseTime, parseActivity, parseEntry, parseLog
    , totalTimeSpent
    ) where

import Control.Applicative
import Data.Char
import Text.Printf
import Text.RawString.QQ
import Text.Trifecta

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int

data Date = Date Year Month Day deriving (Eq)
data Time = Time Hour Minute deriving (Eq)
data Activity = Activity Time String deriving (Eq)
data Entry = Entry Date [Activity] deriving (Eq)
data Log = Log [Entry] deriving (Eq)

instance Show Date where
    show (Date y m d) = printf "%d/%02d/%02d" y m d

instance Show Time where
    show (Time h m) = printf "%02d:%02d" h m

manipTime :: (Int -> Int -> Int) -> Time -> Time -> Time
manipTime f (Time h m) (Time h' m') =
    let minSum = m `f` m'
        (hour', min) = minSum `quotRem` 60
        hour = h `f` h' + hour'
    in Time hour min

addTime :: Time -> Time -> Time
addTime = manipTime (+)

subTime :: Time -> Time -> Time
subTime = manipTime (-)

instance Show Activity where
    show (Activity t s) = show t ++ " " ++ printf "%s" s

instance Show Entry where
    show (Entry d as) = show d ++ "\n" ++ showActivities as
        where showActivities []     = ""
              showActivities (a:as) = show a ++ "\n" ++ showActivities as

instance Show Log where
    show (Log es) = showEntries es
        where showEntries []     = ""
              showEntries (e:es) = show e ++ "\n" ++ showEntries es

example :: String
example = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

eol :: Parser ()
eol = (eof <|> (const () <$> newline)) >> spaces

comment :: Parser ()
comment = string "--" >> manyTill anyChar eol >> return ()

comments :: Parser ()
comments = many comment >> return ()

skipToToken :: Parser ()
skipToToken = spaces >> comments

parseDate :: Parser Date
parseDate = do
    char '#'
    spaces
    year <- read <$> count 4 digit
    char '-'
    month <- read <$> count 2 digit
    char '-'
    day <- read <$> count 2 digit
    return $ Date year month day

parseTime :: Parser Time
parseTime = do
    skipToToken
    hour <- read <$> count 2 digit
    char ':'
    min <- read <$> count 2 digit
    return $ Time hour min

parseActivity :: Parser Activity
parseActivity = do
    skipToToken
    time <- parseTime
    spaces
    -- TODO: Trim trailing whitespaces.
    activity <- manyTill anyChar (comment <|> eol)
    return $ Activity time activity

parseEntry :: Parser Entry
parseEntry = do
    skipToToken
    date <- parseDate
    activities <- many parseActivity
    return $ Entry date activities

parseLog :: Parser Log
parseLog = Log <$> many parseEntry

timeActivities [] = Time 0 0
timeActivities ((Activity t _) : []) = Time 0 0
timeActivities ((Activity t _) : a@(Activity t' _) : as) =
    t' `subTime` t `addTime` timeActivities (a : as)

totalTimeEntry :: Entry -> Time
totalTimeEntry (Entry _ []) = Time 0 0
totalTimeEntry (Entry _ as) = timeActivities as

totalTimeSpent :: Log -> Time
totalTimeSpent (Log []) = Time 0 0
totalTimeSpent (Log es) = timeEntries es
  where
    timeEntries [] = Time 0 0
    timeEntries ((Entry _ as) : es) = timeActivities as `addTime` timeEntries es

