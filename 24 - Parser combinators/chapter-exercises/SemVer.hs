module SemVer where

import Control.Applicative
import Data.Char (digitToInt)
import Data.Maybe (maybeToList)
import Text.Trifecta

data NumberOrString = NOSI Integer | NOSS String deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
    compare (SemVer maj min pat rel _) (SemVer maj' min' pat' rel' _) =
        if maj /= maj' then compare maj maj'
        else if min /= min' then compare min min'
        else if pat /= pat' then compare pat pat'
        else compareRelease rel rel'
        where
            compareRelease []     []     = EQ
            compareRelease []     _      = GT
            compareRelease _      []     = LT
            compareRelease (x:xs) (y:ys) =
                let comp = compare x y
                in if comp == EQ then
                    compareRelease xs ys
                else
                    comp

noLeadingZeroes :: Parser String
noLeadingZeroes = do
    num1 <- oneOf ['1'..'9']
    num2 <- many digit <* notFollowedBy letter
    return $ num1 : num2

numeric :: Parser Integer
numeric = zero <|> (read <$> noLeadingZeroes)
    where zero = (toInteger . digitToInt) <$> (char '0' <* notFollowedBy alphaNum)

version :: Parser Integer
version = numeric

noLeadingZeroString :: Parser String
noLeadingZeroString = do
    head <- notChar '0'
    tail <- many alphaNum
    return $ head : tail

identifier :: Parser NumberOrString
identifier = (NOSI <$> try numeric) <|> (NOSS <$> noLeadingZeroString)

identifiers :: Parser [NumberOrString]
identifiers = identifier `sepBy` char '.'

preRelease :: Parser [NumberOrString]
preRelease = char '-' >> identifiers

buildMetadata :: Parser [NumberOrString]
buildMetadata = char '+' >> identifiers

-- https://semver.org/
parseSemVer :: Parser SemVer
parseSemVer = do
    major <- version
    char '.'
    minor <- version
    char '.'
    patch <- version
    release <- optional preRelease
    metadata <- optional buildMetadata
    return $ SemVer major minor patch (optionalToList release) (optionalToList metadata)
    where
        optionalToList = concat . maybeToList

