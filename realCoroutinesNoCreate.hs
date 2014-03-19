import Coroutine

import Control.Monad.State

type VoidCrt = CoroutineT () () IO ()
type Thing = StateT [() -> VoidCrt] IO ()

runIt :: Thing -> IO ()
runIt th = liftM fst $ runStateT (th >> startCrt) []

coroutine :: VoidCrt -> Thing
coroutine crt = do
    modify (const crt :)

startCrt :: Thing
startCrt = do
    crts <- get
    case crts of
      [] -> return ()
      (crt:_) -> do
        res <- liftIO $ runCoroutineT (crt ())
        case res of
          (Result v) -> return v
          (Yield o k) -> do
            modify (\ cs -> (tail cs) ++ [k])
            startCrt

ex :: VoidCrt
ex = do
    liftIO $ putStrLn "1"
    yield ()
    liftIO $ putStrLn "3"
    yield ()

ex2 :: VoidCrt
ex2 = do
    liftIO $ putStrLn "2"
    yield ()
    liftIO $ putStrLn "4"
    yield ()

runExs = runIt $ (coroutine ex) >> (coroutine ex2)


main :: IO ()
main = runExs
