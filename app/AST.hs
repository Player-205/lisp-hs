module AST where


import Data.Text (Text)
import qualified Data.Text as Text

data LispVal
  = KeyWord KeyWord
  | Symbol  Text
  | String  Text
  | Bool    Bool
  | Number  Number
  | List    [LispVal]


data KeyWord = Div | Times | Minus | Plus
 
data Number
  = I Integer
  | F Double

-- deriving instance Show LispVal
-- deriving instance Show KeyWord
-- deriivng instance Show Number
instance Show Number where
  show (I n) = show n
  show (F n) = show n
instance Show KeyWord where
  show Div   = "/"
  show Times = "*"
  show Minus = "-"
  show Plus  = "+"

instance Show LispVal where
  show (KeyWord word)  = show word
  show (Symbol sym)    = Text.unpack sym
  show (String str)    = show str
  show (Number num)    = show num
  show (Bool True)     = "true"
  show Bool{}          = "false"
  show (List l)        = between '(' ')' . unwords . map prettyShow $ l

prettyShow :: LispVal -> String
prettyShow val@KeyWord {} = show val
prettyShow val@Symbol  {} = show val
prettyShow val@String  {} = show val
prettyShow val@Number  {} = show val
prettyShow val@Bool    {} = show val
prettyShow val@List    {} = ('\n':) .  unlines . map ("  " <>) . lines . show $ val


between :: a -> a -> [a] -> [a]
between s e str = addEnd (s:str)
  where
    addEnd [] = [e]
    addEnd (x:xs) = x : addEnd xs




