import Coroutine

import Control.Monad
import Control.Monad.IO.Class

type PrintC a = CoroutineT Int String IO a

tester :: PrintC [Int]
tester = do
    liftIO $ putStrLn "In tester!"
    x <- yield "What is your favorite number?"
    liftIO $ putStrLn $ "caller told me this " ++ show x
    return [x]



main :: IO ()
main = do
    putStrLn "hi"
    x <- runCoroutineT tester
    res <- handle x
    putStrLn $ "caller finishes: " ++ show res

  where
    handle :: Result Int String IO [Int] -> IO [Int]
    handle (Result ls) = return ls
    handle (Yield o k) = do
      putStrLn $ "callee says: " ++ o
      putStrLn "What is your response? (number please)"
      v <- liftM read getLine
      join $ liftM handle $ runCoroutineT $ k v


