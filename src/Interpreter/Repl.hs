module Interpreter.Repl where

import System.IO
import Control.Applicative
import Control.Monad.IO.Class

import Interpreter.Types



keycodes :: [(String, Key)]
keycodes = [ ("[D", KeyLeft)
    , ("[C", KeyRight)
    , ("[A", KeyUp)
    , ("[B", KeyDown)
    , ("[7", KeyPos1)
    , ("[8", KeyEnd)
    , ("[3", KeyDel)
    -- xterm:
    , ("[1;5D", KeyLeft)
    , ("[1;5C", KeyRight)
    -- rxvt
    , ("[OD", KeyLeft)
    , ("[OC", KeyRight) ]



getChars :: IO String
getChars = loop stdin (return []) =<< hReady stdin
    where
        loop h xs True = loop h (liftA2 (:) (hGetChar h) xs) =<< hReady h
        loop h xs False = reverse <$> xs


parseInput :: String -> Input
parseInput ('\ESC':xs) = Special $ case lookup xs keycodes of
                            Just x -> x
                            Nothing -> KeyUnknown
parseInput ('\n':[])    = Special KeyEnter
parseInput ('\b':[])    = Special KeyBack
parseInput x            = Normal x


getInput :: Repl Input
getInput = liftIO $ parseInput <$> getChars


repl :: Repl ()
repl env = undefined










