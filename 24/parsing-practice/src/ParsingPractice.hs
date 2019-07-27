module ParsingPractice where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' >>= (\x -> eof >> return x)

one' :: Parser b
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' >>= (\x -> eof >> return x)

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

-- | 2)
oneS :: Parser String
oneS = string "1"

oneTwoS :: Parser String
oneTwoS = string "12"

oneTwoThreeS :: Parser String
oneTwoThreeS = string "123"

oneS' :: Parser String
oneS' = string "1" >> stop

oneTwoS' :: Parser String
oneTwoS' = string "12" >> stop

oneTwoThreeS' :: Parser String
oneTwoThreeS' = string "123" >> stop

testParseS :: Parser String -> IO ()
testParseS p = print $ parseString p mempty "123"

-- | 3)
myString :: String -> Parser String
myString = mapM char

oneMS :: Parser String
oneMS = myString "1"

oneTwoMS :: Parser String
oneTwoMS = myString "12"

oneTwoThreeMS :: Parser String
oneTwoThreeMS = myString "123"

main :: IO ()
main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'

    -- 2
    pNL "oneS:"
    testParseS oneS
    pNL "oneS':"
    testParseS oneS'
    pNL "oneTwoS:"
    testParseS oneTwoS
    pNL "oneTwoS':"
    testParseS oneTwoS'
    pNL "oneTwoThreeS:"
    testParseS oneTwoThreeS
    pNL "oneTwoThreeS':"
    testParseS oneTwoThreeS'

    -- 3
    pNL "oneMS:"
    testParseS oneMS
    pNL "oneTwoS:"
    testParseS oneTwoMS
    pNL "oneTwoThreeS:"
    testParseS oneTwoThreeMS

