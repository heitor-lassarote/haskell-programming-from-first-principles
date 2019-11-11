{-# LANGUAGE OverloadedStrings #-}

module Data.Ini where

import           Control.Applicative
import           Data.ByteString     (ByteString)
import           Data.Char           (isAlpha)
import qualified Data.Map            as M
import           Text.Trifecta

newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair $ Header <$> some letter

type Name = String
type Value = String
type Assignments = M.Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf "\n"

skipComments :: Parser ()
skipComments =
    skipMany (do char ';' <|> char '#'
                 skipMany $ noneOf "\n"
                 skipEOL)

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (M.Map Header Assignments) deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ char ' ' <|> char '\n'

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    skipWhitespace
    skipComments
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section -> M.Map Header Assignments -> M.Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return $ Config mapOfSections

