-- Was I supposed to use fold to do all these stuff? Nah, probably not.
module Database where

import Data.Time

data DatabaseItem
    = DbString String
    | DbNumber Integer
    | DbData   UTCTime
    deriving (Eq, Ord, Show)
    
theDatabase
  = [ DbData (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world"
    , DbData (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

-- | Exercise 1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (db:dbs)
  = case db of
        DbData utcTime -> utcTime : filterDbDate dbs
        _              -> filterDbDate dbs

-- | Exercise 2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (db:dbs)
  = case db of
        DbNumber number -> number : filterDbNumber dbs
        _               -> filterDbNumber dbs

-- | Exercise 3
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- | Exercise 4
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

-- | Exercise 5
avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sum numbers) / fromIntegral (length numbers)
           where numbers = filterDbNumber db
