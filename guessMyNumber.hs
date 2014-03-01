import Coroutine

import System.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.IO.Class

data GuessState = GuessState { lowerBound :: Int
                             , upperBound :: Int
                             , guessCount :: Int
                             }
                             deriving (Show)

startGS :: GuessState
startGS = GuessState minBound maxBound 0

type Guessee a = CoroutineT (Maybe Int) String (StateT GuessState (ReaderT Int Identity)) a
runGuessee :: GuessState -> Int -> Guessee Int -> (Result (Maybe Int) String (StateT GuessState (ReaderT Int Identity)) Int, GuessState)
runGuessee st env comp = runIdentity (runReaderT (runStateT (runCoroutineT comp) st) env)

guessManager :: String -> Guessee Int
guessManager msg = do
    num <- lift ask
    x <- yield $ msg ++ "Guess my number!"
    lift $ modify (\ gs -> gs { guessCount = 1 + (guessCount gs) })
    gs <- lift get
    let guess = case x of
                  Nothing -> safeAverage (upperBound gs) (lowerBound gs)
                  Just n -> n
    case compare guess num of
      GT -> do
        lift $ modify (\ gs -> gs { upperBound = min guess (upperBound gs) })
        (GuessState lB uB _) <- lift get
        guessManager $ formatResponse "high" lB uB
      EQ -> return num
      LT -> do
        lift $ modify (\ gs -> gs { lowerBound = max guess (lowerBound gs) })
        (GuessState lB uB _) <- lift get
        guessManager $ formatResponse "low" lB uB
  where
    formatResponse highOrLow lB uB = "Your guess was " ++ highOrLow ++ ". Your range is now between " ++ show lB ++ " and " ++ show uB ++ ". (" ++ show (uB-lB) ++ ")\n"



main :: IO ()
main = do
    putStrLn "hi"
    number <- randomIO
    (num,count) <- handle number $ runGuessee startGS number (guessManager "")
    putStrLn $ "The number was: " ++ show num ++ ", it took " ++ show count ++ " guesses to get it"

  where
    handle _ (Result num,gS) = return (num, guessCount gS)
    handle number (Yield msg k,gS) = do
      putStrLn msg
      putStrLn "What is your response? (number please)"
      v <- getLine
      let guess = case v of
                    "" -> Nothing
                    _ -> Just (read v)
      handle number $ runGuessee gS number (k guess)

safeAverage :: Int -> Int -> Int
safeAverage n1 n2 = let numSum = (fromIntegral n1) + (fromIntegral n2) :: Integer
                    in fromIntegral (div numSum 2)
