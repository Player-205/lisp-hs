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
import Data.Map (Map)
import qualified Data.Map as Map

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

keywords :: Map String LispVal
keywords =  
  Map.insert "false" (Bool False)
  $ Map.insert "true" (Bool True)
  $ Map.fromList $ fmap (\kw -> (show kw,  KeyWord kw)) [minBound .. maxBound]


symbol :: Parser LispVal
symbol = do
  content <- some (noneOf @[] " \r\t\n()")
  case keywords Map.!? content of
    Just val -> pure val
    _ -> pure . Symbol . pack $ content

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
      , rawString
      , list
      , symbol
      ]

parseLisp :: String -> Either String LispVal
parseLisp (pack -> t) = parse lisp' "lisp" t & \case
  Left err -> Left (errorBundlePretty err)
  Right val -> pure val
  where
    lisp' = (some lisp <* eof)  <&> \case
      [val] -> val
      vals  -> List vals