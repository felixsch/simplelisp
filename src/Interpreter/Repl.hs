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


getInputLine :: String -> [String] -> IO String
getInputLine pre buf = putStr pre *> (handle [] 0 =<< getInput)
    where

        handle _  _ (Normal "\EOT") = putStr "\n" *> return "\EOT"
        handle xs bufpos (Normal x) = putStr x *> get (x ++ xs) bufpos

        handle xs _ (Special KeyEnter) = putStr "\n" *> return (reverse xs)
        handle xs bufpos (Special KeyBack) = delChar xs bufpos

        handle x bufpos (Special KeyUp)
            | bufpos +1  ==  buflen = get x bufpos
            | otherwise             = replaceLine pre (buffer !! (bufpos +1)) *> get (buffer !! (bufpos + 1)) (bufpos + 1)
        handle x bufpos (Special KeyDown)
            | bufpos == 0           = get x bufpos
            | otherwise             = replaceLine pre (buffer !! (bufpos -1)) *> get (buffer !! (bufpos - 1)) (bufpos - 1)


        handle xs bufpos _ = get xs bufpos 

        get x bufpos = handle x bufpos =<< getInput

        delChar [] bufpos = get [] bufpos
        delChar xs bufpos = termDelChar >> get (tail xs) bufpos

        buflen = length buffer
        buffer = "" : buf


replaceLine :: String -> String -> IO ()
replaceLine pre re = clearLine *> cursorBackward 255 *>  (putStr $ pre ++ re)


termDelChar :: IO ()
termDelChar = cursorBackward 1 >> clearFromCursorToLineEnd












