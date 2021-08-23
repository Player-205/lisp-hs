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




eval :: LispVal -> StateT Env (ExceptT Text IO) LispVal
eval v@String{} = pure v
eval v@Symbol{} = pure v
eval v@Bool  {} = pure v
eval v@Number{} = pure v
eval v@KeyWord{} = pure v
eval (List((KeyWord kw):xs)) = evalKeyWord kw xs
eval (List (arg:args)) = foldl' (\x y -> x >> eval y) (eval arg) args
eval v@(List []) = pure v



strict :: [LispVal] -> StateT Env (ExceptT Text IO) [LispVal]
strict = mapM eval

evalKeyWord :: KeyWord -> [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evalKeyWord Div    = evaluateDiv    <=< strict
evalKeyWord Times  = evaluateTimes  <=< strict
evalKeyWord Minus  = evaluateMinus  <=< strict
evalKeyWord Plus   = evaluatePlus   <=< strict
evalKeyWord Mod    = evaluateMod    <=< strict
evalKeyWord Concat = evaluateConcat <=< strict
evalKeyWord GT     = evaluateGT     <=< strict
evalKeyWord GE     = evaluateGE     <=< strict
evalKeyWord LT     = evaluateLT     <=< strict
evalKeyWord LE     = evaluateLE     <=< strict
evalKeyWord Eq     = evaluateEq     <=< strict
evalKeyWord NoEq   = evaluateNoEq   <=< strict
evalKeyWord Quote  = evaluateQuote
evalKeyWord TypeOf = evaluateTypeOf <=< strict
evalKeyWord Cons   = evaluateCons   <=< strict
evalKeyWord Car    = evaluateCar    <=< strict
evalKeyWord Cdr    = evaluateCdr    <=< strict
evalKeyWord Cond   = evaluateCond
evalKeyWord Print  = evaluatePrint  <=< strict
evalKeyWord Read   = evaluateRead   <=< strict
evalKeyWord Eval   = evaluateEval   <=< strict


evaluateEval :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateEval [] = throwError "eval: missing an argument"
evaluateEval [x] = eval x
evaluateEval (x:_) = eval x

evaluateRead :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateRead [] = liftIO getLine >>= (parseLisp >>> \case
   Left err -> throwError ("read" <> pack err)
   Right val -> pure val)
evaluateRead args  = evaluatePrint args >> evaluateRead []


evaluatePrint :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluatePrint [] = throwError "print: missing an argument"
evaluatePrint (arg:args) = nil <$ foldl' (\x y -> x >> printVal y) (printVal arg) args
  where printVal = liftIO . putStr . show <=< eval

evaluateCond :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateCond [cond, x, y] = eval cond >>= \case
  Bool True -> eval x
  Bool False -> eval y
  _ -> throwError "cond: conditional argument not a bool"
evaluateCond (cond:x:xs@(_:_)) = eval  cond >>= \case
  Bool True -> eval x
  Bool False -> evaluateCond xs
  _ -> throwError "cond: conditional argument not a bool"
evaluateCond _ = throwError "cond: invalid count of arguments"


evaluateCdr :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateCdr [] = throwError "cdr: missing an argument"
evaluateCdr (List []:_) = pure nil
evaluateCdr (List (_:xs):_) = pure (List xs)
evaluateCdr _ = throwError "cdr: invalid argument"

evaluateCar :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateCar [] = throwError "car: missing an argument"
evaluateCar (List []:_) = throwError "car: empty list"
evaluateCar (List [x]:_) = pure x
evaluateCar (List (x:_):_) = pure x
evaluateCar _ = throwError "car: invalid argument"

evaluateCons :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateCons [] = pure nil
evaluateCons [x, y] = pure case y of
    List l ->  List (x:l)
    y ->  List [x, y]
evaluateCons (x:list) = do
  (List l) <- evaluateCons list
  pure (List (x : l))

typeOf :: LispVal -> Text
typeOf  KeyWord{}   = "KeyWord"
typeOf  Symbol{}    = "Symbol"
typeOf  String{}    = "String"
typeOf  Bool{}      = "Bool"
typeOf (Number I{}) = "Integer"
typeOf (Number F{}) = "Float"
typeOf  List{}      = "List"

evaluateTypeOf :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateTypeOf [] = throwError "typeof: missing an argument"
evaluateTypeOf (x:_) = pure $ String (typeOf x)

evaluateQuote :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateQuote [] = throwError "quote: missing an argument"
evaluateQuote (x:_) = pure x


evaluateNoEq :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateNoEq = boolOp "noEq" (\x y -> pure ( x /= y ))

evaluateEq :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateEq = boolOp "eq" (\x y -> pure ( x == y ))

evaluateLE :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateLE = ordOp (<=) "le"

evaluateLT :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateLT = ordOp  (<) "lt"

evaluateGE :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateGE = ordOp  (>=) "ge"

evaluateGT :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateGT = ordOp  (>) "gt"

boolOp :: (MonadError Text m) => Text -> (b -> b -> m Bool) -> [b] -> m LispVal
boolOp err _ [] = throwError $ err <> ": missing arguments"
boolOp err _ [_] = throwError $ err <> ": missig second argument"
boolOp _ op l = Bool . and <$> zipWithM op l (tail l)

ordOp ::  (forall a . Ord a =>  a -> a -> Bool) -> Text -> [LispVal] -> StateT Env (ExceptT Text IO) LispVal
ordOp op err l = flip (boolOp err) l \x y -> case (x,y) of
  (KeyWord{},_) -> throwError (err <> ": cannot ord keywords")
  (_,KeyWord{}) -> throwError (err <> ": cannot ord keywords")
  (String s1, String s2) -> pure (s1 `op` s2)
  (Number (I n1), Number (I n2)) -> pure (n1 `op` n2)
  (Number (F n1), Number (F n2)) -> pure (n1 `op` n2)
  (Bool   b1, Bool   b2) -> pure (b1 `op` b2)
  (List   l1, List   l2) -> all (\(Bool b) -> b) <$> zipWithM (\x y -> ordOp op err [x,y]) l1 l2
  _ -> throwError (err <> ": cannot match type " <> typeOf x <> " with type " <> typeOf y <> ".")


evaluateConcat :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateConcat [] = throwError "concat: missing an argument"
evaluateConcat [s@String{}] = pure s
evaluateConcat [v] = pure $ String (pack $ show v)
evaluateConcat (String s:xs) = evaluateConcat xs <&> \(String str) -> String (s <> str)
evaluateConcat (x:xs) = evaluateConcat xs <&> \(String str) -> String (pack (show x) <> str)


unwrapNumbers :: [LispVal] -> StateT Env (ExceptT Text IO) [Number]
unwrapNumbers = mapM \case
  Number n -> pure n
  x -> throwError $ typeOf x <> " not a number!"

evaluatePlus :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluatePlus l = unwrapNumbers l >>= \case
  [] ->throwError "plus: missing an argument"
  [x] -> pure $ Number x
  (x:xs) -> pure . Number $ foldl' (numOp (+)) x xs

evaluateMinus :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateMinus l = unwrapNumbers l >>= \case
  [] ->throwError "minus: missing an argument"
  [I x] -> pure $ Number $ I (-x)
  [F x] -> pure $ Number $ F (-x)
  (x:xs) -> pure . Number $ foldl' (numOp (-)) x xs

evaluateTimes :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateTimes l = unwrapNumbers l >>= \case
  [] ->throwError "times: missing an argument"
  [x] -> pure $ Number x
  (x:xs) -> pure . Number $ foldl' (numOp (*)) x xs

evaluateDiv :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
evaluateDiv l = unwrapNumbers l >>= \case
  [] ->throwError "div: missing an argument"
  [x] -> pure $ Number x
  (x:xs) -> pure . Number $ foldl' divOp x xs
  where
    divOp (I x) (I y) = I (x `div` y)
    divOp (F x) (F y) = F (x / y)
    divOp (I x) (F y) = F (fromInteger x / y)
    divOp (F x) (I y) = F (x / fromInteger y)


evaluateMod :: [LispVal] -> StateT Env (ExceptT Text IO) LispVal
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



