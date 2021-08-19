{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module MyParser where

import Data.Text qualified as Text
import Control.Applicative ( Alternative(some, many, (<|>)) )
import Data.Foldable ( asum )
import Control.Monad.State ( MonadPlus, StateT(StateT) )
import Data.Semigroup ( First(First) )
import Control.Monad.Except ( ExceptT(ExceptT), Except )
import Data.Functor.Identity ( Identity(Identity) )
import Text.Printf ( printf )
import Data.Functor ( ($>), (<&>) )
import AST
    ( Number(I, F), KeyWord(Div, Plus, Minus, Times), LispVal(..) )

data Input = Input
  { inputLoc :: Int
  , inputStr :: String
  } deriving (Show, Eq)

data ParserError = ParserError Int String deriving (Show)
  deriving Semigroup via First ParserError

instance Monoid ParserError where
  mempty =  ParserError 0 "empty error"

newtype Parser a = Parser { runParser :: Input -> Either ParserError (a, Input) }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus) via StateT Input (Except ParserError)


(<?>) :: Parser a -> String -> Parser a
parser <?> description = Parser \input -> case runParser parser input of
  (Left (ParserError n _)) -> Left (ParserError n description)
  r -> r

infixr 2 <?>

inputUncons :: Input  -> Maybe (Char, Input)
inputUncons (Input _ [])       = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

char :: Char -> Parser Char
char x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x    = Right (x, ys)
      | otherwise =
        Left $
        ParserError
          (inputLoc input) ("Expected '" <> [x] <> "', but found '" <> [y] <> "'")
    f input =
      Left $
      ParserError (inputLoc input) ("Expected '" <> [x] <> "', but reached end of string")


satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | pred y    = Right (y, ys)
      | otherwise =
        Left $
        ParserError
          (inputLoc input) ("'" <> [y] <> "' don't match predicat")
    f input =
      Left $
      ParserError (inputLoc input) "Expected match of predicat, but reached end of string"

eof :: Parser ()
eof = Parser \(Input n input) -> case input of
    [] -> pure ((), Input n "")
    _ ->  Left $ ParserError n ("Expected end of string, but reached \"" <> input <> "\"")

nonChar :: Char -> Parser Char
nonChar ch = satisfy (/= ch) <?> "Not expected '" <> [ch] <> "', but reached"

string :: String -> Parser String
string str = Parser \input ->
  runParser
    (traverse char str <?> "Expected \"" <> str <> "\", but found \"" <> inputStr input <> "\"")
    input

nonOf :: [Char] -> Parser Char
nonOf l = satisfy (`notElem` l) <?> "Not expected '" <> l <> "', but reached"

choice :: Foldable t => t (Parser a) -> Parser a
choice = asum

oneOf :: [Char] -> Parser Char
oneOf  = choice . map char

number :: Parser Char
number = oneOf "0123456789"

intParser :: Parser Integer
intParser = read <$> some number

floatParser :: Parser Double
floatParser = do
  intPart <- some number
  _ <- char '.'
  fractPart <- some number
  pure (read (intPart <> "." <> fractPart))

numberParser :: Parser LispVal
numberParser = Number <$> choice
  [ F <$> floatParser
  , I <$> intParser
  ]

space :: Parser ()
space = many (oneOf " \n\t") $> ()



stringParser :: Parser LispVal
stringParser = String . Text.pack <$> (char '"' *> many (escape <|> nonOf "\\\"") <* char '"')
  where
    escape = char '\\' *> (oneOf "0abfnrtv\"\\" <&> toEscape)
    toEscape x = case x of
      { '0'  ->  '\0';      'a'  ->  '\a'
      ; 'b'  ->  '\b';      'f'  ->  '\f'
      ; 'n'  ->  '\n';      'r'  ->  '\r'
      ; 't'  ->  '\t';      'v'  ->  '\v'
      ; '"'  ->  '\"';      '\\' ->  '\\';
      _ -> error "stringParser: imposible"
      }


boolParser :: Parser LispVal
boolParser = choice
  [ Bool True <$ string "true"
  , Bool False <$ string "false"
  ]

keywordParser :: Parser LispVal
keywordParser = KeyWord <$> choice
  [ Plus  <$ char '+'
  , Minus <$ char '-'
  , Times <$ char '*'
  , Div   <$ char '/'
  ]

symbolParser :: Parser LispVal
symbolParser = Symbol . Text.pack <$> some (nonOf " ()")

listParser :: Parser LispVal
listParser = List <$> (char '(' *> some lispParser <* char ')' )

lispParser :: Parser LispVal
lispParser = space *> parse <* space
  where
    parse = choice
      [ numberParser
      , stringParser
      , boolParser
      , keywordParser
      , listParser
      , symbolParser
      ]


parseLisp :: String -> String
parseLisp = testParser (lispParser <* eof)

testParser :: Show a => Parser a -> String -> String
testParser parser input = case runParser parser (Input 0 input) of
  Left (ParserError loc cause) -> printf "in %d: \n %s\n" loc cause
  (Right (val, Input _ [])) ->  show val
  (Right (val, Input n str)) -> show val <> printf "\nrest in %d: %s\n" n str