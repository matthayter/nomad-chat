
module TransformerTest where


import           Data.List
import           Data.Maybe
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Functor.Identity

data MyError = MyError
    deriving Show

type TestOp1 = ExceptT MyError (StateT Int IO)
type TestOp2 = StateT Int (ExceptT MyError IO)

runOp1 :: Show a => TestOp1 a -> IO ()
runOp1 op = do
    let s = runExceptT op
    let v = runStateT s 0
    (eVal, finalState) <- v
    putStrLn "Final state:"
    putStrLn (show finalState)
    case eVal of
        Left e -> do
            putStrLn "Error:"
            putStrLn (show e)
        Right a -> do
            putStrLn "Value:"
            putStrLn (show a)

runOp2 :: Show a => TestOp2 a -> IO ()
runOp2 op = do
    let e = runStateT op 0
    let ioEitherVal = runExceptT e
    eitherVal <- ioEitherVal
    case eitherVal of
        Left e -> do
            putStrLn "Error:"
            putStrLn (show e)
        Right (a, fState) -> do
            putStrLn "Final state:"
            putStrLn (show fState)
            putStrLn "Value:"
            putStrLn (show a)

testOp1_1 :: TestOp1 String
testOp1_1 = do
    put 1
    throwError MyError
    put 2
    return "okay!"

testOp1_2 :: TestOp1 String
testOp1_2 =
    let
        f = do
            liftIO $ putStrLn "cleanup..."
            liftIO $ putStrLn "State is:"
            s <- get
            liftIO $ putStrLn $ show s
            return "cleaned!"
    in
        cleanup testOp1_1 f

testOp2_1 :: TestOp2 String
testOp2_1 = do
    put 1
    put 2
    throwError MyError
    return "okay!"

testOp2_2 :: TestOp2 String
testOp2_2 = 
    let
        f = do
            liftIO $ putStrLn "recovering..."
            liftIO $ putStrLn "State is:"
            s <- get
            liftIO $ putStrLn $ show s
    in
        cleanup testOp2_1 f

-- 'cleanup op always' sequences 'cleanup' into 'op' regardless of whether 'op' is an error
cleanup :: MonadError e m => m a -> m b -> m a
cleanup maybeFailure always = do
    catchError maybeFailure passthru
    val <- maybeFailure
    always
    return val
    where
        passthru someErr = always >> throwError someErr