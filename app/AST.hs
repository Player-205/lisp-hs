module AST where


import Prelude hiding (GT, LT)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor (first)
import Data.Functor

data LispVal
  = KeyWord KeyWord
  | Symbol  Text
  | String  Text
  | Bool    Bool
  | Number  Number
  | List    [LispVal]
  deriving Eq


data KeyWord 
  = Div 
  | Times 
  | Minus 
  | Plus 
  | Mod
  | Concat
  | GT
  | GE
  | LT
  | LE
  | Eq
  | NoEq
  | Quote
  | TypeOf
  | Cons
  | Car
  | Cdr
  | Cond
  | Print
  | Read
  | Eval
 deriving Eq

data Number
  = I Integer
  | F Double
  deriving Eq



-- deriving instance Show LispVal
-- deriving instance Show KeyWord
-- deriving instance Show Number

instance Show Number where
  show (I n) = show n
  show (F n) = show n


instance Show KeyWord where
  show Div    = "/"  
  show Times  = "*"  
  show Minus  = "-"  
  show Plus   = "+"  
  show Mod    = "mod"
  show Concat = "++"  
  show GT     = ">"  
  show GE     = ">="  
  show LT     = "<"  
  show LE     = "<="  
  show Eq     = "="  
  show NoEq   = "!="  
  show Quote  = "quote"
  show TypeOf = "typeof"
  show Cons   = "cons"
  show Car    = "car"
  show Cdr    = "cdr"
  show Cond   = "cond"
  show Print  = "print"
  show Read   = "read"
  show Eval   = "eval"


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




runLisp :: Env -> StateT Env (ExceptT Text IO) LispVal -> IO (String, Env) 
runLisp env action = runExceptT (first show  <$> runStateT action env) <&> \case
  Left err -> (Text.unpack err, env)
  Right val -> val

type Env  = ()

nullEnv :: ()
nullEnv = ()

nil :: LispVal
nil = List []