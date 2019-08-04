import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- 1 & 2)
rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . flip (-) 1

-- 3 & 4)
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

-- 5)
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
    putStrLn $ "Hi: " ++ show r
    return $ r + 1

-- 6)
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \r -> do
    putStrLn $ "Hi: " ++ show r
    return (show r, r + 1)

