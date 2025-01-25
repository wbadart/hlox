module Foo where

{- HLINT ignore "Eta reduce" -}

import Bluefin.Writer

import Control.Monad (forM_)
import Control.Monad.Trans.State.Strict
import Data.Functor ((<&>), ($>), void)
import Data.List (foldl')
import Data.STRef (newSTRef)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (isEOF)

-- Main action!
-- ==========

type Error = (Int, String)

tokens :: String -> Either Error [Token]
tokens script = evalState go (ScanState{scanLine=1, start=0, current=0, result=[]})
  where
    go :: State ScanState (Either Error [Token])
    go = do
      done <- atEnd
      if done
        then do
          state@ScanState{result=result, scanLine=line} <- get
          return $ Right $ result <> [Token EOF "" () line]
        else do
          state@ScanState{current=current} <- get
          put state{start=current}
          error <- scanToken
          maybe go (return . Left) error

    scanToken :: State ScanState (Maybe Error)
    scanToken = do
      c <- advance
      case c of
        '(' -> addToken LEFT_PAREN  $> Nothing
        ')' -> addToken RIGHT_PAREN $> Nothing
        '{' -> addToken LEFT_BRACE  $> Nothing
        '}' -> addToken RIGHT_BRACE $> Nothing
        ',' -> addToken COMMA       $> Nothing
        '.' -> addToken DOT         $> Nothing
        '-' -> addToken MINUS       $> Nothing
        '+' -> addToken PLUS        $> Nothing
        ';' -> addToken SEMICOLON   $> Nothing
        '*' -> addToken STAR        $> Nothing
        _ -> do
          ScanState{scanLine=line} <- get
          return $ Just (line, "Unexpected character")

    atEnd :: State ScanState Bool
    atEnd = gets current <&> (>= length script)

    advance :: State ScanState Char
    advance = do
      state@ScanState{current=current} <- get
      put state{current=current + 1}
      return (script !! current)

    addToken :: TokenType -> State ScanState ()
    addToken tokenType = do
      state@ScanState{start=start, current=current, result=result, scanLine=line} <- get
      let text = substring start current script
      put state{result=result <> [Token tokenType text () line]}

    substring :: Int -> Int -> String -> String
    substring start end string = take (end - start) $ drop start string

data ScanState = ScanState
  { scanLine :: Int
  , start :: Int
  , current :: Int
  , result :: [Token]
  }
  deriving Show

data Token = Token
  { tokenType :: TokenType
  , lexeme :: String
  , literal :: ()  -- not sure what this is yet
  , line :: Int
  }
  deriving Show

data TokenType =
  -- Single-character tokens.
  LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |
  COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR |

  -- One or two character tokens.
  BANG | BANG_EQUAL |
  EQUAL | EQUAL_EQUAL |
  GREATER | GREATER_EQUAL |
  LESS | LESS_EQUAL |

  -- Literals.
  IDENTIFIER | STRING | NUMBER |

  -- Keywords.
  AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR |
  PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE |

  EOF
  deriving Show

-- Plumbing
-- ==========

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
    Right lexemes -> do
      forM_ lexemes print
      pure ExitSuccess
    Left (errorLine, errorMessage) -> do
      err errorLine errorMessage
      pure (ExitFailure 65)

-- Helpers
-- ==========

err :: Int -> String -> IO ()
err line message = report line "" message

report :: Int -> String -> String -> IO ()
report line where' message =
  putStrLn $ "[" <> show line <> "] Error" <> where' <> ": " <> message
