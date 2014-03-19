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
      case (res,createdCrts) of
        (Result v, []) -> return ()
        (Yield o k, []) -> pipeState (VoidCrt $ k ())
        (Result v, (c:cs)) -> do
          put cs
          pipeState (c ())
        (Yield o k, (c:cs)) -> do
          put (cs ++ [VoidCrt . k])
          pipeState (c ())


ex :: VoidCrt
ex = VoidCrt $ do
    coroutine ex2
    liftIO $ putStrLn "1"
    yield ()
    liftIO $ putStrLn "3"
    yield ()

ex2 :: VoidCrt
ex2 = VoidCrt $ do
    liftIO $ putStrLn "2"
    coroutine ex3
    yield ()
    liftIO $ putStrLn "4"
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

printOut = liftIO . putStrLn

main :: IO ()
main = startCrt [] ex
