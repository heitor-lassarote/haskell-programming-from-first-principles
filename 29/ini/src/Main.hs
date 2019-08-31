module Main where

import           Data.List           (isSuffixOf)
import qualified Data.Map            as M
import qualified System.Directory    as SD
import qualified System.Environment  as SE
import           System.Exit         (exitFailure)
import qualified System.IO           as SIO
import           Text.Trifecta

import           Data.Ini

getFilesByExtension :: FilePath -> String -> IO [FilePath]
getFilesByExtension dir extension
  = SD.listDirectory dir
    >>= traverse (SD.findFilesWith (return . isSuffixOf extension) [dir])
    >>= return . concat

parseFiles :: [FilePath] -> IO (M.Map FilePath (Result Config))
parseFiles files = do
    inis <- sequence (SIO.readFile <$> files)
    let configs = parseString parseIni mempty <$> inis
    return $ M.fromList $ zip files configs

main :: IO ()
main = do
    args <- SE.getArgs
    dir <-
        case length args of
            0 -> SD.getCurrentDirectory
            1 -> return $ head args
            _ -> putStrLn "Incorrect number of arguments" >> exitFailure
    files <- getFilesByExtension dir ".ini"
    configs <- parseFiles files
    print configs

