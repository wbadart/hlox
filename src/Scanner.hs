-- Chapter 4
module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (elemIndex, findIndex, isPrefixOf, tails)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

tokens :: String -> Either [Error] [Token]
tokens script =
  case tokens' 1 script of
    ([], tokens) -> Right tokens
    (errors, _)  -> Left errors

tokens' :: Int -> String -> ([Error], [Token])

tokens' line ""               = ([], [Token EOF         (StringLexeme "")  line])
tokens' line (c@'(' : script) = ([], [Token LEFT_PAREN  (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@')' : script) = ([], [Token RIGHT_PAREN (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@'{' : script) = ([], [Token RIGHT_BRACE (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@'}' : script) = ([], [Token LEFT_BRACE  (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@',' : script) = ([], [Token COMMA       (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@'.' : script) = ([], [Token DOT         (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@'-' : script) = ([], [Token MINUS       (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@'+' : script) = ([], [Token PLUS        (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@';' : script) = ([], [Token SEMICOLON   (StringLexeme [c]) line]) <> tokens' line script
tokens' line (c@'*' : script) = ([], [Token STAR        (StringLexeme [c]) line]) <> tokens' line script

tokens' line ('!' : '=' : script) = ([], [Token BANG_EQUAL    (StringLexeme "!=") line]) <> tokens' line script
tokens' line ('=' : '=' : script) = ([], [Token EQUAL_EQUAL   (StringLexeme "==") line]) <> tokens' line script
tokens' line ('<' : '=' : script) = ([], [Token LESS_EQUAL    (StringLexeme "<=") line]) <> tokens' line script
tokens' line ('>' : '=' : script) = ([], [Token GREATER_EQUAL (StringLexeme ">=") line]) <> tokens' line script
tokens' line ('!'       : script) = ([], [Token BANG_EQUAL    (StringLexeme "!")  line]) <> tokens' line script
tokens' line ('='       : script) = ([], [Token EQUAL_EQUAL   (StringLexeme "=")  line]) <> tokens' line script
tokens' line ('<'       : script) = ([], [Token LESS          (StringLexeme "<")  line]) <> tokens' line script
tokens' line ('>'       : script) = ([], [Token GREATER       (StringLexeme ">")  line]) <> tokens' line script

tokens' line ('/' : '/' : script) =
  let endOfComment = charIndexOrEnd '\n' script
  in tokens' line (drop endOfComment script)

tokens' line ('/' : '*' : script) =
  let endOfComment = substrIndexOrEnd "*/" script + length "*/"
  in tokens' line (drop endOfComment script)

tokens' line ('/' : script) = ([], [Token SLASH (StringLexeme "/") line]) <> tokens' line script

tokens' line (' ' : script)  = tokens' line       script
tokens' line ('\r' : script) = tokens' line       script
tokens' line ('\t' : script) = tokens' line       script
tokens' line ('\n' : script) = tokens' (line + 1) script

tokens' line ('"' : script) =
  case elemIndex '"' script of
    Nothing -> ([Error line "Unterminated string"], []) <> tokens' line script
    Just closeQuote ->
      let string = take closeQuote script
          nLinebreaks = elemCount '\n' string
      in ([], [Token STRING (StringLexeme string) line])
          <> tokens' (line + nLinebreaks) (drop (closeQuote + 1) script)

tokens' line script@(c : _) | isDigit c =
  case span isDigit script of
    (wholePart, '.' : next : rest) | isDigit next ->
      let (decimalPart, rest') = span isDigit (next : rest)
      in ([], [Token NUMBER (NumLexeme (read $ wholePart <> "." <> decimalPart)) line]) <> tokens' line rest'
    (wholePart, rest) ->
      ([], [Token NUMBER (NumLexeme (read wholePart)) line]) <> tokens' line rest

tokens' line script@(c : _) | isAlpha c || c == '_' =
  let (ident, rest) = span isAlphaNum script
      type_ = fromMaybe IDENTIFIER (M.lookup ident keywords)
  in ([], [Token type_ (StringLexeme ident) line]) <> tokens' line rest

tokens' line (c : script) = ([Error line ("Unexpected character '" <> [c] <> "'")], []) <> tokens' line script

-- ==========

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

-- ==========

substring :: Int -> Int -> String -> String
substring start end string = take (end - start) $ drop start string

charIndexOrEnd :: Eq c => c -> [c] -> Int
charIndexOrEnd c string =
  let len = length string
  in min (fromMaybe len (elemIndex c string)) len

substrIndexOrEnd :: Eq c => [c] -> [c] -> Int
substrIndexOrEnd substr = fromMaybe 0 . findIndex (substr `isPrefixOf`) . tails

elemCount :: Eq a => a -> [a] -> Int
elemCount x = length . filter (== x)
