-- NOTE: This wasn't completely tested, and some parts still don't match the
-- original specification, however, it should work well for many basic examples.
module Dot where

import Control.Applicative
import Control.Monad (void)
import Data.Char
import Data.Maybe (isJust, maybeToList)
import Data.Ratio

import Text.Trifecta

type Strict = Bool
type ID = String

data GraphType = GraphType | DigraphType deriving (Eq, Show)

data Graph = Graph Strict GraphType (Maybe ID) [Stmt] deriving (Eq, Show)

data Stmt
    = NodeStmt     Node
    | EdgeStmt     Edge
    | AttrStmt     Attr
    | IDStmt       (ID, ID)
    | SubgraphStmt Subgraph
    deriving (Eq, Show)

data Structure = GraphStruct | NodeStruct | EdgeStruct deriving (Eq, Show)

data Attr = Attr Structure AttrList deriving (Eq, Show)
type AttrList = [AList]
type AList = [(ID, ID)]

type NodeOrSubgraph = Either NodeID Subgraph

data Edge = Edge NodeOrSubgraph EdgeRHS AttrList deriving (Eq, Show)
data EdgeRHS = EdgeRHS EdgeOP NodeOrSubgraph [EdgeRHS] deriving (Eq, Show)

data Node = Node NodeID [AttrList] deriving (Eq, Show)
data NodeID = NodeID ID (Maybe Port) deriving (Eq, Show)
data Port
    = IDPort ID (Maybe CompassPt)
    | PTPort CompassPt
    deriving (Eq, Show)

data Subgraph = Subgraph (Maybe ID) [Stmt] deriving (Eq, Show)
data CompassPt = N | NE | E | SE | S | SW | W | NW | C | X deriving (Eq, Show)

data EdgeOP = Directed | Undirected deriving (Eq, Show)

type Numeral = Either Rational Integer

ciChar :: Char -> Parser Char
ciChar c = char (toLower c) <|> char (toUpper c) <?> show [c]

ciString :: String -> Parser String
ciString s = try (traverse ciChar s) <?> show s

comment :: Parser ()
comment =
    string "//"
    *> manyTill anyChar (eof <|> void newline)
    *> spaces
    <?> "comment"

inlineComment :: Parser ()
inlineComment =
    string "/*"
    *> manyTill anyChar (string "*/")
    *> spaces
    <?> "inline comment"

ws :: Parser ()
ws = spaces *> skipMany inlineComment <?> "whitespace or comment"

ws1 :: Parser ()
ws1 =
    (some space *> ws)
    <|> void (some inlineComment)
    <?> "whitespace or comment"

parseInteger :: Parser Integer
parseInteger = read <$> some digit <?> "integer"

parseNumeral :: Parser Numeral
parseNumeral = parse <?> "numeral"
  where
    parse = do
        minus <- isJust <$> optional (char '-')
        value <- decimal <|> intDecimal
        if minus
            then return $ negate <$> value
            else return value
    decimal = char '.' *> ((Left . (% 1)) <$> parseInteger)
    intDecimal = do
        int <- parseInteger
        frac <- optional $ char '.' *> (read <$> option "0" (many digit))
        case frac of
          Just frac' -> return $ Left $ int % frac'
          Nothing    -> return $ Right int

parseID :: Parser ID
parseID =
    some (char '_' <|> alphaNum)
    <|> (show <$> parseNumeral)
    <|> between (char '"') (char '"') (many $ notChar '"')
    <?> "ID"
--  where
--    quotedString = string "\\\""

graph :: Parser Graph
graph = parse <?> "graph"
 where
    parse = do
        ws
        strict <- isJust <$> optional (ciString "strict")
        ws
        gType <- ciString "graph" <|> ciString "digraph"
        ws
        id <- optional parseID
        stmts <- parseBracketStmtList
        return $ Graph strict (graphType gType) id stmts
    graphType "graph"   = GraphType
    graphType "digraph" = DigraphType

parseBracketStmtList :: Parser [Stmt]
parseBracketStmtList =
    ws *> between (char '{') (char '}') parseStmtList <?> "statement list"

parseIdId :: Parser (ID, ID)
parseIdId = parse <?> "ID attribution"
  where
    parse = do
        ws
        id <- parseID
        ws
        char '='
        ws
        id' <- parseID
        return (id, id')

parseStmtList :: Parser [Stmt]
parseStmtList = parse <?> "statement list"
  where
    parse = do
        ws
        stmts <- parseStmt `sepEndBy` (try (ws *> char ';' *> ws) <|> ws1)
        ws
        return stmts

parseStmt :: Parser Stmt
parseStmt = choice
    [ IDStmt       <$> try parseIdId
    , NodeStmt     <$> try parseNodeStmt
    , EdgeStmt     <$> try parseEdgeStmt
    , AttrStmt     <$> try parseAttrStmt
    , SubgraphStmt <$> parseSubgraph
    ] <?> "node, edge, attribution, ID assignment or subgraph statement"

parseEdgeStmt :: Parser Edge
parseEdgeStmt = parse <?> "edge statement"
  where
    parse = do
        graph <- (Left <$> parseNodeID) <|> (Right <$> parseSubgraph)
        ws
        edge <- parseEdgeRHS
        attrs <- (concat . maybeToList) <$> optional (try parseAttrList)
        return $ Edge graph edge attrs

parseEdgeRHS :: Parser EdgeRHS
parseEdgeRHS = parse <?> "edge RHS (--> or --)"
  where
    parse = do
        edgeOP <- parseEdgeOP
        ws
        graph <- (Left <$> parseNodeID) <|> (Right <$> parseSubgraph)
        edge <- optional $ try (ws *> parseEdgeRHS)
        return $ EdgeRHS edgeOP graph (maybeToList edge)

parseEdgeOP :: Parser EdgeOP
parseEdgeOP = toEdgeOp <$> (string "->" <|> string "--") <?> "edge operation"
  where
    toEdgeOp "->" = Directed
    toEdgeOp "--" = Undirected

parseAttrList :: Parser AttrList
parseAttrList = parse <?> "attribution list"
  where
    parse = do
        ws
        char '['
        alist <- parseAList
        ws
        char ']'
        attrs <- (concat . maybeToList) <$> optional (try parseAttrList)
        return $ alist : attrs

parseAList :: Parser AList
parseAList =
    (parseIdId `sepBy` (ws *> char ';'))
    <|> (parseIdId `sepBy` (ws *> char ','))
    <?> "ID assignment list"

parseAttrStmt :: Parser Attr
parseAttrStmt = parse <?> "attribution statement"
  where
    structure "graph" = GraphStruct
    structure "node"  = NodeStruct
    structure "edge"  = EdgeStruct
    parse = do
        ws
        struct <- structure <$> choice [ciString "graph", ciString "node", ciString "edge"]
        attrs <- parseAttrList
        return $ Attr struct attrs

parseNodeStmt :: Parser Node
parseNodeStmt =
    Node <$> parseNodeID <*> (maybeToList <$> optional parseAttrList)
    <?> "node statement"

parseNodeID :: Parser NodeID
parseNodeID = NodeID <$> parseID <*> optional parsePort <?> "node ID"

parsePort :: Parser Port
parsePort = parsePortID <|> parsePortPT <?> "port"
  where
    parsePortID = do
        char ':'
        id <- parseID
        compass <- char ':' *> optional parseCompassPt
        return $ IDPort id compass
    parsePortPT = PTPort <$> (char ':' *> parseCompassPt)

parseSubgraph :: Parser Subgraph
parseSubgraph = parse <?> "subgraph"
  where
    parse = do
    try $ ciString "subgraph"
    id <- optional parseID
    stmts <- parseBracketStmtList
    return $ Subgraph id stmts

parseCompassPt :: Parser CompassPt
parseCompassPt = parse <?> "compass point"
  where
    toCompass "N"  = N
    toCompass "NE" = NE
    toCompass "E"  = E
    toCompass "SE" = SE
    toCompass "S"  = S
    toCompass "SW" = SW
    toCompass "W"  = W
    toCompass "NW" = NW
    toCompass "C"  = C
    toCompass "_"  = X
    parse = choice $ map (fmap toCompass . ciString)
        ["N", "NE", "E", "SE", "S", "SW", "W", "NW", "C", "_"]

