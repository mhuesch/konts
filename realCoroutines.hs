import Coroutine

import Control.Monad.State

newtype VoidCrt = VoidCrt {
                    runCrt :: CST
                  }

type CST =  CoroutineT () () (StateT [() -> VoidCrt] IO) ()

coroutine :: VoidCrt -> CST
coroutine crt = do
    modify (const crt :)

startCrt :: [() -> VoidCrt] -> VoidCrt -> IO ()
startCrt suspendedList crt = liftM fst $ runStateT (pipeState crt) suspendedList
  where
    pipeState crt = do
      res <- runCoroutineT (runCrt crt)
      createdCrts <- get
      case createdCrts of
        [] -> case res of
          Result v -> return ()
          Yield o k -> pipeState (VoidCrt $ k ())
        (c:cs) -> do
          case res of
            Result v -> put cs
            Yield o k -> put (cs ++ [VoidCrt . k])
          pipeState (c ())


ex :: VoidCrt
ex = VoidCrt $ do
    coroutine ex2
    printOut "1"
    yield ()
    printOut "3"
    yield ()

ex2 :: VoidCrt
ex2 = VoidCrt $ do
    printOut "2"
    coroutine ex3
    yield ()
    printOut "4"
    yield ()

ex3 :: VoidCrt
ex3 = VoidCrt $ do
  printOut "hi"
  yield ()
  printOut "chickens"
  yield ()
  printOut "tots"
  yield ()
  printOut "I'm last"
  yield ()
  return ()

printOut :: String -> CoroutineT () () (StateT [() -> VoidCrt] IO) ()
printOut = liftIO . putStrLn

main :: IO ()
main = startCrt [] ex
