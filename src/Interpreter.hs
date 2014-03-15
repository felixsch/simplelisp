module Interpreter (interpreter) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Applicative

import System.Console.ANSI
import System.Posix.Terminal
import System.Posix.IO
import System.IO
import qualified Data.Map as M
import Interpreter.Types
import Interpreter.Builtin
import Interpreter.Eval
import Interpreter.Repl
import Types
import Parser


builtins :: Lookup
builtins = Lookup builtinSymbols builtinFunctions


newEnv :: Env
newEnv = Env builtins [] []


interpreter :: IO ()
interpreter = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering
    attrs <- getTerminalAttributes stdOutput
    setTerminalAttributes stdOutput (withoutMode attrs EnableEcho) Immediately

    putStrLn "type :q to quit"

    loop newEnv [] =<< getInputLine ">> " []

    setTerminalAttributes stdOutput (withoutMode attrs EnableEcho) Immediately

    hSetBuffering stdout LineBuffering
    hSetBuffering stdin  LineBuffering
    where
        loop _ _ ":q"   = return ()
        loop _ _ "\EOT" = return ()
        loop e b x      = do
            case parseLisp x of
                Left err   -> putStrLn $ "Parse Error: " ++ show err
                Right expr -> do
                    xx <- (foldM evalator e expr)
                    next <- getInputLine ">> " b
                    loop xx (next:b) next

evalator :: Env -> LispExp -> IO Env
evalator env expr = do
                e <- runErrorT $ runStateT (evaluate expr) env
                case e of
                    Left err -> return env <* (putStrLn $ "Eval Error: " ++ err)
                    Right (x, ne) -> return ne <*  (putStrLn $ show x)
