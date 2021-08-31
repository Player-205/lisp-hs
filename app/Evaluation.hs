{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Evaluation where


import AST
import Data.Foldable

import Control.Monad
import Control.Monad.State
import Prelude hiding (LT, GT)
import Control.Monad.Except
import Data.Text (Text, pack)
import Parser (parseLisp)
import Control.Arrow
import Data.Functor
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import System.IO


eval :: (MonadReader Env m, MonadError Text m, MonadIO m, MonadFail m) =>  LispVal -> m LispVal
eval v@String{}    = pure v
eval (Symbol name) = getVal name
eval v@Bool  {}    = pure v
eval v@Number{}    = pure v
eval (KeyWord Env) = Environment <$> join (asks newFrame)
eval v@KeyWord{}   = pure v
eval v@Lambda{}    = pure v
eval v@Macro{}     = pure v
eval v@(List [])   = pure v
eval Environment{} = throwError "cannot evaluate environment"
eval (List (arg:args)) = do
  head <- eval arg
  case head of
    KeyWord kw -> evalKeyWord kw args


    Lambda env argNames body -> do
      args <- strict args
      env <- newFrame env
      withNewEnv env (runLambda argNames args body)
    Macro argNames body ->  do
      dict <- boundNames argNames args
      let extractedBody = extractMacro dict body
      eval extractedBody
    _ -> strict args <&> \case
      [] -> head
      xs -> last xs

extractMacro :: Map Text LispVal -> LispVal -> LispVal
extractMacro macroNames s@(Symbol name) = fromMaybe s (macroNames Map.!? name)
extractMacro macroNames (List l) = List $ map (extractMacro macroNames) l
extractMacro _ v = v

boundNames :: MonadError Text f => [Text] -> [LispVal] -> f (Map Text LispVal)
boundNames  = go Map.empty
  where
    go dict [] [] = pure dict
    go _ names [] = throwError $ "Missing " <> pack (show (length names)) <> " arguments in macro"
    go dict [] _  = pure dict
    go dict [name] [arg] = pure $ Map.insert name arg dict
    go dict [name] args = pure $ Map.insert name (List args) dict
    go dict (name:names) (arg:args) = let d = Map.insert name arg dict in d `seq` go d names args

runLambda :: (MonadReader Env m, MonadError Text m, MonadIO m, MonadFail m) =>
  [Text] -> [LispVal] -> LispVal -> m LispVal
runLambda [] _ body = eval body
runLambda names [] body =  asks \env -> Lambda env names body
runLambda [name] [arg] body = defVal name arg >> eval body
runLambda [name] args body = defVal name (List args) >> eval body
runLambda (name:names) (arg:args) body = defVal name arg >> runLambda names args body

strict :: (MonadReader Env m, MonadError Text m, MonadIO m, MonadFail m) =>  [LispVal] -> m [LispVal]
strict = mapM eval

evalKeyWord :: (MonadReader Env m, MonadError Text m, MonadIO m, MonadFail m) =>  KeyWord -> [LispVal] -> m LispVal
evalKeyWord Div        = evaluateDiv    <=< strict
evalKeyWord Times      = evaluateTimes  <=< strict
evalKeyWord Minus      = evaluateMinus  <=< strict
evalKeyWord Plus       = evaluatePlus   <=< strict
evalKeyWord Mod        = evaluateMod    <=< strict
evalKeyWord Concat     = evaluateConcat <=< strict
evalKeyWord GT         = evaluateGT     <=< strict
evalKeyWord GE         = evaluateGE     <=< strict
evalKeyWord LT         = evaluateLT     <=< strict
evalKeyWord LE         = evaluateLE     <=< strict
evalKeyWord Eq         = evaluateEq     <=< strict
evalKeyWord NoEq       = evaluateNoEq   <=< strict
evalKeyWord Quote      = evaluateQuote
evalKeyWord TypeOf     = evaluateTypeOf <=< strict
evalKeyWord Cons       = evaluateCons   <=< strict
evalKeyWord Car        = evaluateCar    <=< strict
evalKeyWord Cdr        = evaluateCdr    <=< strict
evalKeyWord Cond       = evaluateCond
evalKeyWord Print      = evaluatePrint  <=< strict
evalKeyWord Read       = evaluateRead   <=< strict
evalKeyWord Eval       = evaluateEval   <=< strict

evalKeyWord Def        = evaluateDef
evalKeyWord Set        = evaluateSet
evalKeyWord Sym        = evaluateSym
evalKeyWord LambdaDecl = evaluateLambdaDecl
evalKeyWord MacroDecl  = evaluateMacroDecl
evalKeyWord Env        = \case [] -> pure (KeyWord Env) ; args -> last <$> strict args
evalKeyWord EvalIn     = evaluateEvalIn <=< strict


evaluateDef
 , evaluateSet
 , evaluateSym
 , evaluateLambdaDecl
 , evaluateMacroDecl
 , evaluateEval
 , evaluateRead
 , evaluatePrint
 , evaluateCond
 , evaluateCdr
 , evaluateCar
 , evaluateCons
 , evaluateTypeOf
 , evaluateQuote
 , evaluateNoEq
 , evaluateEq
 , evaluateLE
 , evaluateLT
 , evaluateGE
 , evaluateGT
 , evaluateConcat
 , evaluatePlus
 , evaluateMinus
 , evaluateTimes
 , evaluateDiv
 , evaluateMod 
 , evaluateEvalIn :: (MonadReader Env m, MonadError Text m, MonadIO m, MonadFail m) => [LispVal] -> m LispVal


evaluateEvalIn (Environment env:args:_) = withNewEnv env (eval args)
evaluateEvalIn _ = throwError "eval-in: invalid arguments"

writeEnv :: (MonadError Text m, MonadFail m, MonadIO m, MonadReader Env m) =>
  Text -> (Text -> LispVal -> m b) -> [LispVal] -> m LispVal

writeEnv _ act [Symbol s, val] = nil <$ (act s =<< eval val)
writeEnv fn act [name, val] = evaluateSym [name] >>= \n ->  writeEnv fn act [n, val]

writeEnv fn act (Symbol s:val:xs) = (act s =<< eval val) >> writeEnv fn act xs
writeEnv fn act (name:val:xs) = evaluateSym [name] >>= \n ->  writeEnv fn act (n:val:xs)
writeEnv fn _ _ = throwError $ fn <> ": invalid argument count"

evaluateDef = writeEnv "def" defVal

evaluateSet = writeEnv "set!" setVal

evaluateSym [] = throwError "symbol: missing an argument"
evaluateSym (x:_) =  eval x <&> \case 
  String s -> Symbol s
  x -> Symbol . pack . show $ x

evaluateLambdaDecl [List args, body] = do
  argNames <- asSymbols "lambda" args
  env <- ask
  pure (Lambda env argNames body)
evaluateLambdaDecl (List args: body@(_:_)) = evaluateLambdaDecl [List args, List body]
evaluateLambdaDecl _ = throwError "Invalid lambda declaration: missing a body"


asSymbols :: MonadError Text f => Text -> [LispVal] -> f [Text]
asSymbols _ [] = pure []
asSymbols fn (Symbol s:xs) = (s:) <$> asSymbols fn xs
asSymbols fn (x:_) = throwError $ "Invalid " <> fn <>" declaration: " <> pack (show x) <> " not a symbol!"

evaluateMacroDecl [List args, body] = do
  argNames <- asSymbols "macro" args
  pure (Macro argNames body)
evaluateMacroDecl (List args: body@(_:_)) = evaluateMacroDecl [List args, List body]
evaluateMacroDecl _ = throwError "Invalid macro declaration: missing a body"


evaluateEval [] = throwError "eval: missing an argument"
evaluateEval [x] = eval x
evaluateEval (x:_) = eval x

evaluateRead [] = liftIO getLine >>= (parseLisp >>> \case
   Left err -> throwError ("read" <> pack err)
   Right val -> pure val)
evaluateRead args  = evaluatePrint args >> evaluateRead []


evaluatePrint [] = throwError "print: missing an argument"
evaluatePrint (arg:args) = do
  foldl' (\x y -> x >> printVal y) (printVal arg) args
  liftIO $ hFlush stdout
  pure nil
  where printVal = liftIO . putStr . (\case (String s) -> Text.unpack s; v -> show v) <=< eval

evaluateCond [cond, x, y] = eval cond >>= \case
  Bool True -> eval x
  Bool False -> eval y
  _ -> throwError "cond: conditional argument not a bool"
evaluateCond (cond:x:xs@(_:_)) = eval  cond >>= \case
  Bool True -> eval x
  Bool False -> evaluateCond xs
  _ -> throwError "cond: conditional argument not a bool"
evaluateCond _ = throwError "cond: invalid count of arguments"


evaluateCdr [] = throwError "cdr: missing an argument"
evaluateCdr (List []:_) = pure nil
evaluateCdr (List (_:xs):_) = pure (List xs)
evaluateCdr _ = throwError "cdr: invalid argument"

evaluateCar [] = throwError "car: missing an argument"
evaluateCar (List []:_) = throwError "car: empty list"
evaluateCar (List [x]:_) = pure x
evaluateCar (List (x:_):_) = pure x
evaluateCar _ = throwError "car: invalid argument"

evaluateCons [] = pure nil
evaluateCons [x, y] = pure case y of
    List l ->  List (x:l)
    y ->  List [x, y]
evaluateCons (x:list) = do
  (List l) <- evaluateCons list
  pure (List (x : l))

typeOf :: LispVal -> Text
typeOf  KeyWord{}     = "KeyWord"
typeOf  Symbol{}      = "Symbol"
typeOf  String{}      = "String"
typeOf  Bool{}        = "Bool"
typeOf (Number I{})   = "Integer"
typeOf (Number F{})   = "Float"
typeOf  List{}        = "List"
typeOf  Lambda{}      = "Lambda"
typeOf  Macro{}       = "Macro"
typeOf  Environment{} = "Environment"

evaluateTypeOf [] = throwError "typeof: missing an argument"
evaluateTypeOf (x:_) = pure $ String (typeOf x)

evaluateQuote [] = throwError "quote: missing an argument"
evaluateQuote (x:_) = pure x


evaluateNoEq = boolOp "noEq" (\x y -> pure ( x /= y ))

evaluateEq = boolOp "eq" (\x y -> pure ( x == y ))

evaluateLE = ordOp (<=) "le"

evaluateLT = ordOp  (<) "lt"

evaluateGE = ordOp  (>=) "ge"

evaluateGT = ordOp  (>) "gt"

boolOp :: (MonadError Text m) => Text -> (b -> b -> m Bool) -> [b] -> m LispVal
boolOp err _ [] = throwError $ err <> ": missing arguments"
boolOp err _ [_] = throwError $ err <> ": missig second argument"
boolOp _ op l = Bool . and <$> zipWithM op l (tail l)

ordOp :: (MonadReader Env m, MonadError Text m, MonadIO m) => (forall a . Ord a =>  a -> a -> Bool) -> Text -> [LispVal] -> m LispVal
ordOp op err l = flip (boolOp err) l \x y -> case (x,y) of
  (KeyWord{},_) -> throwError (err <> ": cannot ord keywords")
  (_,KeyWord{}) -> throwError (err <> ": cannot ord keywords")
  (String s1, String s2) -> pure (s1 `op` s2)
  (Number n1, Number n2) -> pure (n1 `op` n2)
  (Bool   b1, Bool   b2) -> pure (b1 `op` b2)
  (List   l1, List   l2) -> all (\(Bool b) -> b) <$> zipWithM (\x y -> ordOp op err [x,y]) l1 l2
  _ -> throwError (err <> ": cannot match type " <> typeOf x <> " with type " <> typeOf y <> ".")


evaluateConcat [] = throwError "concat: missing an argument"
evaluateConcat [s@String{}] = pure s
evaluateConcat [v] = pure $ String (pack $ show v)
evaluateConcat (String s:xs) = evaluateConcat xs <&> \(String str) -> String (s <> str)
evaluateConcat (x:xs) = evaluateConcat xs <&> \(String str) -> String (pack (show x) <> str)


unwrapNumbers :: (MonadReader Env m, MonadError Text m, MonadIO m) => [LispVal] -> m [Number]
unwrapNumbers = mapM \case
  Number n -> pure n
  x -> throwError $ typeOf x <> " not a number!"

evaluatePlus l = unwrapNumbers l >>= \case
  [] ->throwError "plus: missing an argument"
  [x] -> pure $ Number x
  (x:xs) -> pure . Number $ foldl' (numOp (+)) x xs

evaluateMinus l = unwrapNumbers l >>= \case
  [] ->throwError "minus: missing an argument"
  [I x] -> pure $ Number $ I (-x)
  [F x] -> pure $ Number $ F (-x)
  (x:xs) -> pure . Number $ foldl' (numOp (-)) x xs

evaluateTimes l = unwrapNumbers l >>= \case
  [] ->throwError "times: missing an argument"
  [x] -> pure $ Number x
  (x:xs) -> pure . Number $ foldl' (numOp (*)) x xs


evaluateDiv l = unwrapNumbers l >>= \case
  [] ->throwError "div: missing an argument"
  [x] -> pure $ Number x
  (x:xs) -> pure . Number $ foldl' divOp x xs
  where
    divOp (I x) (I y) = I (x `div` y)
    divOp (F x) (F y) = F (x / y)
    divOp (I x) (F y) = F (fromInteger x / y)
    divOp (F x) (I y) = F (x / fromInteger y)


evaluateMod l = unwrapNumbers l >>= \case
  [] -> throwError "mod: missing an argument"
  [x] -> pure $ Number x
  (x:xs) -> pure . Number $ foldl' modOp x xs
  where
    modOp (I x) (I y) = I (x `mod` y)
    modOp (F x) (F y) = I (round x `mod` round y)
    modOp (I x) (F y) = I (x `mod` round y)
    modOp (F x) (I y) = I (round x `mod` y)

numOp :: (forall a. Num a => a -> a -> a) -> Number -> Number -> Number
numOp op (I x) (I y) = I (x `op` y)
numOp op (F x) (F y) = F (x `op` y)
numOp op (I x) (F y) = F (fromInteger x `op` y)
numOp op (F x) (I y) = F (x `op` fromInteger y)



