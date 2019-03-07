module StopVowelStop where

stops :: String
stops  = "pbtdkg"

vowels :: String
vowels = "aeiou"

-- | a)
stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(s, v, s') | s <- stops, v <- vowels, s' <- stops]

-- | b)
stopVowelStop' :: [(Char, Char, Char)]
stopVowelStop' = [('p', v, s') | v <- vowels, s' <- stops]

-- | c)
nouns :: [String]
nouns
  = [ "ear"
    , "pizza"
    , "dog"
    , "Haskell"
    , "WinGHCi"
    , "function"
    , "bed"
    , "piano"
    , "Bach"
    , "car"
    ]
    
verbs :: [String]
verbs
  = [ "fucks"
    , "eats"
    , "codes"
    , "sleeps"
    , "drives"
    , "plays"
    , "does"
    ]
    
nounVerbNoun :: [(String, String, String)]
nounVerbNoun = [(n, v, n') | n <- nouns, v <- verbs, n' <- nouns]