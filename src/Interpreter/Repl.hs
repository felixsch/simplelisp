module Interpreter.Repl
    (getInputLine)
    where


import System.IO
import System.Console.ANSI
import Control.Applicative
import Data.Maybe (fromMaybe)
import Interpreter.Types



keycodes :: [(String, Key)]
keycodes = [ ("[D", KeyLeft)
    , ("[C", KeyRight)
    , ("[A", KeyUp)
    , ("[B", KeyDown)
    , ("[7", KeyPos1)
    , ("OH", KeyPos1)
    , ("OF", KeyEnd)
    , ("[8", KeyEnd)
    , ("[3", KeyDel)
    -- xterm:
    , ("[1;5D", KeyLeft)
    , ("[1;5C", KeyRight)
    -- rxvt
    , ("[OD", KeyLeft)
    , ("[OC", KeyRight) ]




getChars :: IO String
getChars = do
    x <- getChar
    loop [x]
    where
        loop xs = do
            r <- hReady stdin
            case r of 
              False -> return $ reverse xs
              True -> do
                    x <- getChar
                    loop (x:xs)

parseInput :: String -> Input
parseInput ('\ESC':xs)  = Special $ fromMaybe KeyUnknown (lookup xs keycodes)
parseInput ('\DEL':[])  = Special KeyBack
parseInput ('\n':[])    = Special KeyEnter
parseInput ('\t':[])    = Special KeyTab
parseInput ('\EOT':[])  = Normal "\EOT"
parseInput x            = Normal x


getInput :: IO Input
getInput = parseInput <$> getChars


getInputLine :: String -> IO String
getInputLine pre = putStr pre *> (handle [] =<< getInput)
    where

        handle _ (Normal "\EOT") = putStr "\n" *> return "\EOT"
        handle xs (Normal x) = putStr x *> get (x ++ xs)

        handle xs (Special KeyEnter) = putStr "\n" *> return (reverse xs)
        handle xs (Special KeyBack) = delChar xs


        handle xs _ = get xs 

        get x = handle x =<< getInput

        delChar [] = get []
        delChar xs = termDelChar >> get (tail xs)


termDelChar :: IO ()
termDelChar = cursorBackward 1 >> clearFromCursorToLineEnd












