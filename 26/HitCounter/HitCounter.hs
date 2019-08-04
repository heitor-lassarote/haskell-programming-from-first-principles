-- Part (most) of solution taken from:
-- https://github.com/BoeingX/haskell-programming-from-first-principles/blob/master/src/MonadTransformers/ChapterExercises/HitCounter.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           System.Environment (getArgs)
import           Web.Scotty.Trans

data Config =
    Config
    { counts :: IORef (M.Map Text Integer)
    , prefix :: Text
    }

type Scotty  = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m =
    -- It is safe to use ! as the element is guaranteed to be present.
    let m' = M.insertWith (\_ a -> a + 1) k 1 m
     in (m', m' M.! k)

updateCounter :: Text -> Config -> IO Integer
updateCounter k c = do
    let counter = counts c
    (m, n) <- bumpBoomp k <$> readIORef counter
    writeIORef counter m
    return n

app :: Scotty ()
app = get "/:key" $ do
    unprefixed <- param "key"
    prefix' <- lift $ ReaderT $ return . prefix
    let key' = mappend prefix' unprefixed
    newInteger <- lift $ ReaderT $ updateCounter key'
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter $ TL.pack prefixArg
        runR = flip runReaderT config
    scottyT 3000 runR app

