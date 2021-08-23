module Parser where


import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text

import AST
import Data.Void
import Data.Function ((&))
import Data.Functor ((<&>))

import Prelude hiding (LT, GT)

type Parser = Parsec Void Text

number :: Parser LispVal
number = Number <$> do 
  minus <- optional (char '-')
  case minus of
    Nothing -> numberP id
    _ ->  numberP (* (-1))
  where
   numberP :: (forall a . Num a => a -> a) -> Parser Number
   numberP f = choice
      [ F . f <$> try L.float
      , I . f <$> L.decimal
      ] 


space :: Parser ()
space = L.space 
  space1 
  (L.skipLineComment ";") 
  (L.skipBlockComment "{-" "-}")

rawString :: Parser LispVal
rawString =  String . pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

bool :: Parser LispVal
bool = choice
  [ Bool True <$ string "true"
  , Bool False <$ string "false"
  ]

keyword :: Parser LispVal
keyword = KeyWord <$> choice
  [       Div    <$ char   '/'
  ,       Times  <$ char   '*'
  ,       Minus  <$ char   '-'
  , try $ Concat <$ string "++"
  ,       Plus   <$ char   '+'
  ,       Mod    <$ string "mod"
  , try $ GE     <$ string ">="
  ,       GT     <$ char   '>'
  , try $ LE     <$ string "<="
  ,       LT     <$ string "<"
  ,       Eq     <$ char   '='
  ,       NoEq   <$ string "!="
  ,       Quote  <$ string "quote"
  ,       TypeOf <$ string "typeof"
  ,       Cons   <$ string "cons"
  ,       Car    <$ string "car"
  ,       Cdr    <$ string "cdr"
  ,       Cond   <$ string "cond"
  ,       Print  <$ string "print"
  ,       Read   <$ string "read"
  ,       Eval   <$ string "eval"
  ]

symbol :: Parser LispVal
symbol = Symbol . pack <$> some (noneOf @[] " \r\t\n()")

list :: Parser LispVal
list = List <$>
  ((char '(' *> manyTill lisp (char ')'))
  <|>
  char '\'' *> (lisp <&> (\x -> [KeyWord Quote, x])))

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

parseLisp :: String -> Either String LispVal
parseLisp (pack -> t) = parse lisp' "lisp" t & \case
  Left err -> Left (errorBundlePretty err)
  Right val -> pure val
  where
    lisp' = (some lisp <* eof)  <&> \case
      [val] -> val
      vals  -> List vals