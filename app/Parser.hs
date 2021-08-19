module Parser where


import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text

import AST
import Data.Void
import Data.Function ((&))

type Parser = Parsec Void Text

number :: Parser LispVal
number = Number <$> choice
  [ F <$> try L.float
  , I <$> L.decimal
  ] <* space1

rawString :: Parser LispVal
rawString =  String . pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

bool :: Parser LispVal
bool = choice
  [ Bool True <$ string "true"
  , Bool False <$ string "false"
  ]

keyword :: Parser LispVal
keyword = KeyWord <$> choice
  [ Plus  <$ char '+'
  , Minus <$ char '-'
  , Times <$ char '*'
  , Div   <$ char '/'
  ]

symbol :: Parser LispVal
symbol = Symbol . pack <$> some (noneOf @[] " \r\t\n()")

list :: Parser LispVal
list = List <$> (char '(' *> someTill lisp (char ')'))

lisp :: Parser LispVal
lisp = space *> parser <* space
  where
    parser = 
      choice 
      [ try number
      , try bool
      , try keyword
      ,     rawString
      ,     list
      ,     symbol
      ]

parseLisp :: String -> String
parseLisp (pack -> t) = parse (lisp <* eof) "lisp" t & \case 
  Right l -> show l
  Left err -> errorBundlePretty err