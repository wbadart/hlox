module Main where

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (isEOF)

import Scanner (tokens)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    []       -> runPrompt
    [script] -> runFile script
    _ -> do
      putStrLn "usage: jlox [script]"
      exitWith (ExitFailure 64)

runPrompt :: IO ()
runPrompt = do
  putStr "> "
  done <- isEOF
  if done
    then pure ()
    else do
      run =<< getLine
      runPrompt

runFile :: FilePath -> IO ()
runFile path = do
  putStrLn $ "run file " <> path
  exitWith =<< run =<< readFile path

run :: String -> IO ExitCode
run script =
  case tokens script of
    Right tokens -> do
      forM_ tokens print
      pure ExitSuccess
    Left errors -> do
      forM_ errors print
      pure (ExitFailure 65)
