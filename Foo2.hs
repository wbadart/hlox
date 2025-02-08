{-# LANGUAGE ScopedTypeVariables #-}

module Foo2 where

import Bluefin.Eff
import Bluefin.State
import Bluefin.Writer

import Control.Monad (forM_, when)
import Data.Bool (bool)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Function ((&))
import Data.Functor ((<&>), void)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (isEOF)
import qualified Data.Map.Strict as M

import Debug.Trace

-- Main event!
-- ==========

tokens :: String -> Either [Error] [Token]
tokens script =
  case runPureEff $
    evalState (ScanState 0 0 1 []) $ \s ->
    runWriter $ \w -> tokens' s w script
  of
    (tokens, []) -> Right tokens
    (tokens, errors) -> Left errors

tokens'
  :: (e_state :> es, e_writer :> es)
  => State ScanState e_state
  -> Writer [Error] e_writer
  -> String
  -> Eff es [Token]
tokens' s w script = do
  done <- atEnd s
  if done
    then do
      ScanState{scanResult=tokens, scanLine=line} <- get s
      return (tokens <> [Token EOF (StringLexeme "") line])
    else do
      state@ScanState{scanCurrent=current} <- get s
      put s state{scanStart=current}
      scanToken s w
      tokens' s w script
  where
    atEnd s = do
      ScanState{scanCurrent=current} <- get s
      return (current >= length script)

    scanToken s w = do
      c <- readChar s
      state <- get s
      case c of
        '(' -> addToken s LEFT_PAREN
        ')' -> addToken s RIGHT_PAREN
        '{' -> addToken s LEFT_BRACE
        '}' -> addToken s RIGHT_BRACE
        ',' -> addToken s COMMA
        '.' -> addToken s DOT
        '-' -> addToken s MINUS
        '+' -> addToken s PLUS
        ';' -> addToken s SEMICOLON
        '*' -> addToken s STAR
        '!' -> addToken s . bool BANG    BANG_EQUAL    =<< match s '='
        '=' -> addToken s . bool EQUAL   EQUAL_EQUAL   =<< match s '='
        '<' -> addToken s . bool LESS    LESS_EQUAL    =<< match s '='
        '>' -> addToken s . bool GREATER GREATER_EQUAL =<< match s '='

        '/' -> do
          comment <- match s '/'
          if comment
            then do
              let eatComment = do
                    next <- peek s
                    done <- atEnd s
                    when (next /= '\n' && not done) $ do
                      advance s
                      eatComment
              eatComment
            else addToken s SLASH

        ' '  -> pure ()
        '\r' -> pure ()
        '\t' -> pure ()
        '\n' -> do
          state@ScanState{scanLine=line} <- get s
          put s state{scanLine=line + 1}

        '"'                       -> string s w
        c | isDigit c             -> number s
        c | isAlpha c || c == '_' -> identifier s

        _ -> do
          ScanState{scanLine=line} <- get s
          tell w [Error line ("Unexpected character '" <> [c] <> "'.")]

    readChar s = currentChar s <* advance s
    advance s = do
      state@ScanState{scanCurrent=current} <- get s
      put s state{scanCurrent=current + 1}

    addToken s tokenType = do
      state@ScanState{scanResult=tokens, scanLine=line} <- get s
      text <- currentSlice s
      put s state{scanResult=tokens <> [Token tokenType (StringLexeme text) line]}

    addToken' s tokenType text = do
      state@ScanState{scanResult=tokens, scanStart=start, scanCurrent=current, scanLine=line} <- get s
      put s state{scanResult=tokens <> [Token tokenType text line]}

    currentSlice s = do
      ScanState{scanResult=tokens, scanStart=start, scanCurrent=current, scanLine=line} <- get s
      return (substring start current script)

    substring :: Int -> Int -> String -> String
    substring start end string = take (end - start) $ drop start string

    match s expected = do
      done <- atEnd s
      if done
        then return False
        else do
          current <- currentChar s
          if current /= expected
            then return False
            else do
              advance s & void
              return True

    currentChar s = do
      ScanState{scanCurrent=current} <- get s
      return (script !! current)

    peek s = do
      done <- atEnd s
      if done
        then return '\0'
        else currentChar s

    peekNext s = do
      ScanState{scanCurrent=current} <- get s
      if current + 1 >= length script
        then return '\0'
        else return (script !! (current + 1))

    string s w = do
      let loop = do
            c <- peek s
            done <- atEnd s
            when (c /= '"' && not done) $ do
              when (c == '\n') $ do
                state@ScanState{scanLine=line} <- get s
                put s state{scanLine=line + 1}
              advance s
              loop
      loop
      done <- atEnd s
      if done
        then do
          ScanState{scanLine=line} <- get s
          tell w [Error line "Unterminated string"]
        else do
          advance s
          ScanState{scanStart=start, scanCurrent=current} <- get s
          let value = substring (start + 1) (current - 1) script
          addToken' s STRING (StringLexeme value)

    number s = do
      let loop = do
            c <- peek s
            when (isDigit c) $ do
              advance s
              loop
      loop
      c <- peek s
      c' <- peekNext s
      when (c == '.' && isDigit c') $ do
        advance s
        let loop = do
              c <- peek s
              when (isDigit c) $ do
                advance s
                loop
        loop
      text <- currentSlice s
      addToken' s NUMBER $ NumLexeme (read text)

    identifier s = do
      let loop = do
            c <- peek s
            when (isAlphaNum c) $ do
              advance s
              loop
      loop
      text <- currentSlice s
      let type_ = fromMaybe IDENTIFIER (M.lookup text keywords)
      addToken s type_

data ScanState = ScanState
  { scanStart :: Int
  , scanCurrent :: Int
  , scanLine :: Int
  , scanResult :: [Token]
  }
  deriving Show

data Token = Token
  { tokenType :: TokenType
  , tokenLexeme :: Lexeme
  , tokenLine :: Int
  }
  deriving Show

data Lexeme
  = StringLexeme String
  | NumLexeme Double
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

keywords :: M.Map String TokenType
keywords = M.fromList
  [ ("and",     AND)
  , ("class",   CLASS)
  , ("else",    ELSE)
  , ("false",   FALSE)
  , ("for",     FOR)
  , ("fun",     FUN)
  , ("if",      IF)
  , ("nil",     NIL)
  , ("or",      OR)
  , ("print",   PRINT)
  , ("return",  RETURN)
  , ("super",   SUPER)
  , ("this",    THIS)
  , ("true",    TRUE)
  , ("var",     VAR)
  , ("while",   WHILE)
  ]

data Error = Error
  { errorLine :: Int
  , errorMessage :: String
  }
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
    Right tokens -> do
      forM_ tokens print
      pure ExitSuccess
    Left errors -> do
      forM_ errors print
      pure (ExitFailure 65)
