module Date where

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
                 deriving (Ord, Show)
type Day = Int
data Date = Date DayOfWeek Day

instance Eq DayOfWeek where
    (==) Sun Sun = True
    (==) Mon Mon = True
    (==) Tue Tue = True
    (==) Wed Wed = True
    (==) Thu Thu = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) _   _   = False
    
instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth')
            = weekday    == weekday'
           && dayOfMonth == dayOfMonth'
