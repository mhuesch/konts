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
startCrt suspendedList crt = do
    (res, createdCrts) <- runStateT (runCoroutineT (runCrt crt)) suspendedList
    case res of
      (Result v) -> do
        case createdCrts of
          [] -> return ()
          (c:cs) -> startCrt cs (c ())
      (Yield o k) -> do
        case createdCrts of
          [] -> startCrt createdCrts (VoidCrt $ k ())
          (c:cs) -> startCrt (cs ++ [VoidCrt . k]) (c ())

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
