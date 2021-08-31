{-# OPTIONS_GHC -Wno-name-shadowing #-}
module AST where


import Prelude hiding (GT, LT)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State
import Control.Monad.Except
import Data.Functor
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader


data LispVal
  = KeyWord KeyWord
  | Symbol  Text
  | String  Text
  | Bool    Bool
  | Number  Number
  | List    [LispVal]
  | Macro [Text] LispVal
  | Lambda Env [Text] LispVal
  | Environment Env
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
  | Def
  | Set
  | Sym
  | LambdaDecl
  | MacroDecl
  | Env
  | EvalIn
 deriving (Eq, Enum, Bounded)

data Number
  = I Integer
  | F Double

instance Eq Number where
  (I i1) == (I i2) = i1 == i2
  (F f1) == (F f2) = f1 == f2
  (F f)  == (I i) = f == fromInteger i
  i == f = f == i

instance Ord Number where
  compare (I i1) (I i2) = compare i1 i2
  compare (F f1) (F f2) = compare f1 f2
  compare (F f) (I i) = compare f (fromInteger i)
  compare (I i) (F f) = compare (fromInteger i) f



-- deriving instance Show LispVal
-- deriving instance Show KeyWord
-- deriving instance Show Number

instance Show Number where
  show (I n) = show n
  show (F n) = show n


instance Show KeyWord where
  show Div        = "/"
  show Times      = "*"
  show Minus      = "-"
  show Plus       = "+"
  show Mod        = "mod"
  show Concat     = "++"
  show GT         = ">"
  show GE         = ">="
  show LT         = "<"
  show LE         = "<="
  show Eq         = "="
  show NoEq       = "!="
  show Quote      = "quote"
  show TypeOf     = "typeof"
  show Cons       = "cons"
  show Car        = "car"
  show Cdr        = "cdr"
  show Cond       = "cond"
  show Print      = "print"
  show Read       = "read"
  show Eval       = "eval"
  show Def        = "def"
  show Set        = "set!"
  show Sym        = "symbol"
  show LambdaDecl = "lambda"
  show MacroDecl  = "macro"
  show Env        = "env"
  show EvalIn     = "eval-in"


instance Show LispVal where
  show (KeyWord word)       = show word
  show (Symbol sym)         = Text.unpack sym
  show (String str)         = show str
  show (Number num)         = show num
  show (Bool True)          = "true"
  show Bool{}               = "false"
  show (List l)             = between '(' ')' . unwords . map show $ l
  show (Macro args body)    =  unwords ["macro", between '(' ')' . Text.unpack . Text.unwords $ args, show body]
  show (Lambda _ args body) = unwords ["lambda", between '(' ')' . Text.unpack . Text.unwords $ args, show body]
  show Environment{} = ""
prettyShow :: LispVal -> String
prettyShow val@KeyWord     {} = show val
prettyShow val@Symbol      {} = show val
prettyShow val@String      {} = show val
prettyShow val@Number      {} = show val
prettyShow val@Bool        {} = show val
prettyShow val@Macro       {} = show val
prettyShow val@Lambda      {} = show val
prettyShow val@Environment {} = show val
prettyShow val@List        {} = ('\n':) .  unlines . map ("  " <>) . lines . show $ val


between :: a -> a -> [a] -> [a]
between s e str = addEnd (s:str)
  where
    addEnd [] = [e]
    addEnd (x:xs) = x : addEnd xs




runLisp :: Env -> ReaderT Env (ExceptT Text IO) LispVal -> IO (Either Text LispVal)
runLisp env action = runExceptT (runReaderT action env) 


type Frame = IORef (Map Text (IORef LispVal))

type Env  = [Frame]


nullEnv :: MonadIO m => m Env
nullEnv = newFrame []

newFrame :: MonadIO m => Env -> m Env
newFrame env = liftIO $ newIORef Map.empty <&> (: env)

getLastFrame :: MonadReader Env s => s Frame
getLastFrame = asks head

withNewEnv :: MonadReader r m => r -> m a -> m a
withNewEnv env = local (const env)


getValFromFrame :: MonadIO m => Frame -> Text -> m (Maybe LispVal)
getValFromFrame frame name = do
  dict <- liftIO $ readIORef frame
  liftIO . traverse readIORef $ dict Map.!? name

updateFrameVal :: MonadIO m => Frame -> Text -> LispVal -> m Bool
updateFrameVal frame name val = do
  dict <- liftIO $ readIORef frame
  case  dict Map.!? name of
    Just pos -> liftIO (writeIORef pos val) $> True
    _ -> pure False

defVal :: (MonadReader Env s, MonadIO s) => Text -> LispVal -> s ()
defVal name val = do
  frame <- getLastFrame
  mutableVal <- liftIO $ newIORef val
  liftIO $ modifyIORef frame (Map.insert name mutableVal)

getVal :: (MonadReader Env m, MonadIO m, MonadError Text m) => Text -> m LispVal
getVal name = do
  env <- ask
  case env of
    [] -> throwError  $ "Variable " <> name <> " not in scope"
    (x:xs) -> do
      val <- getValFromFrame x name
      case val of
        Just val -> pure val
        _ -> withNewEnv xs $ getVal name

setVal :: (MonadReader Env m, MonadError Text m, MonadIO m) => Text -> LispVal -> m ()
setVal name val = do
  env <- ask
  case env of
    [] -> throwError  $ "Variable " <> name <> " not in scope"
    (x:xs) -> do
      isVariableInFrame <- updateFrameVal x name val
      if isVariableInFrame
        then pure ()
        else withNewEnv xs $ setVal name val

nil :: LispVal
nil = List []





