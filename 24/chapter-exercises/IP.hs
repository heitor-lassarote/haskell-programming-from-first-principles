module IP where

import Control.Applicative
import Data.Bits
import Data.Maybe (catMaybes)
import Data.Word
import Numeric (readHex)
import Text.Printf
import Text.Trifecta

data IPAddress = IPAddress Word32 deriving (Eq, Ord)
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

instance Show IPAddress where
    show (IPAddress ip) =
        let v1 = (ip .&. 0xFF000000) `shiftR` 24
            v2 = (ip .&. 0x00FF0000) `shiftR` 16
            v3 = (ip .&. 0x0000FF00) `shiftR`  8
            v4 = (ip .&. 0x000000FF) `shiftR`  0
         in printf "%d.%d.%d.%d" v1 v2 v3 v4

instance Show IPAddress6 where
    show (IPAddress6 w1 w2) =
        let v1 = (w1 .&. 0xFFFF000000000000) `shiftR` 48
            v2 = (w1 .&. 0x0000FFFF00000000) `shiftR` 32
            v3 = (w1 .&. 0x00000000FFFF0000) `shiftR` 16
            v4 = (w1 .&. 0x000000000000FFFF) `shiftR`  0
            v5 = (w2 .&. 0xFFFF000000000000) `shiftR` 48
            v6 = (w2 .&. 0x0000FFFF00000000) `shiftR` 32
            v7 = (w2 .&. 0x00000000FFFF0000) `shiftR` 16
            v8 = (w2 .&. 0x000000000000FFFF) `shiftR`  0
         in printf "%X:%X:%X:%X:%X:%X:%X:%X" v1 v2 v3 v4 v5 v6 v7 v8

countMany :: Int -> Parser a -> Parser [a]
countMany n p
    | n < 0 = fail "must be at least 0"
    | otherwise = catMaybes <$> count n (optional p)

countSome :: Int -> Parser a -> Parser [a]
countSome n p
    | n <= 0 = fail "must be at least 1"
    | otherwise = (:) <$> p <*> countMany (n - 1) p

parseIPv4 :: Parser IPAddress
parseIPv4 = do
    v1 <- countSome 3 digit
    char '.'
    v2 <- countSome 3 digit
    char '.'
    v3 <- countSome 3 digit
    char '.'
    v4 <- countSome 3 digit

    let v1' = (read v1) `shiftL` 24
        v2' = (read v2) `shiftL` 16
        v3' = (read v3) `shiftL`  8
        v4' = (read v4) `shiftL`  0
        adr = v1' .|. v2' .|. v3' .|. v4'

    return $ IPAddress adr

expand :: [String] -> [String]
expand xs =
    let c = 8 - length (filter (not . null) xs)
     in concatMap (\x -> if null x then replicate c "0" else [x]) xs

parseIPv6 :: Parser IPAddress6
parseIPv6 = do
    values <- (countMany 4 hexDigit) `sepEndBy` char ':'

    let values' = map (fst . head . readHex) $ expand values
        (w1, w2) = splitAt 4 values'

    return $ IPAddress6 (toWord64 w1) (toWord64 w2)
    where
        toWord64 xs =
            let (x1, x2, x3, x4) = get4 xs
                x1' = x1 `shiftL` 48
                x2' = x2 `shiftL` 32
                x3' = x3 `shiftL` 16
                x4' = x4 `shiftL`  0
             in x1' .|. x2' .|. x3' .|. x4'
        get4 (x1 : x2 : x3 : x4 : []) = (x1, x2, x3, x4)

ipv4ToIpv6 :: IPAddress -> IPAddress6
ipv4ToIpv6 (IPAddress ip) = IPAddress6 0 $ fromIntegral ip
