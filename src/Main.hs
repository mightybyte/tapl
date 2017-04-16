module Main where

------------------------------------------------------------------------------
import           Data.Char
import qualified Data.Text as T
import           System.Environment
import           System.IO
------------------------------------------------------------------------------
import           UArith.Eval
import           UArith.Parse
import           UArith.Types
------------------------------------------------------------------------------


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
      [file] -> do
        prog <- readFile file
        parseAndEval $ T.pack prog
      [] -> doInteractive
      _ -> putStrLn "Must supply zero or one argument"

doInteractive = do
    putStr "arith> "
    mstr <- getLine
    case mstr of
      "quit" -> return ()
      str -> do
        parseAndEval $ T.pack str
        doInteractive

parseAndEval prog = do
    case parse prog of
      Left e -> do
        putStrLn "Parse error"
        print e
      Right t -> do
        putStrLn "Parsed program to term:"
        print t
        putStrLn "Evaluating..."
        print $ eval t
