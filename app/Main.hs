{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE BangPatterns #-}
module Main where


import Data.Text qualified as Text
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Map     qualified as Map
import Text.Printf ( printf )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import Data.Foldable ( Foldable(foldl') )
import Control.Monad.IO.Class
import System.Directory

import Parser ( parseLisp ) 
import AST
import Data.Function
import Evaluation


main :: IO ()
main = do
  env <- nullEnv
  putStrLn $ unlines [ "0o154 0o151 0o163 0o160 0o055 0o150 0o163"]
  putStrLn =<< evaluateLisp env =<< readFile "std.lisp"
  runInputT defaultSettings (loop env 0 "")

loop :: Env -> Int -> String -> InputT IO ()
loop  env count' code = getInputLine (prompt count') >>= \case
  Nothing -> loop env count' code
  Just (words -> (":q": _)) -> outputStrLn "Goodbye!"
  Just (words -> [":l"]) -> outputStrLn "Missing a file name" >> loop env count' code 
  Just (words -> (":l": file:_)) -> do
    exist <- liftIO $ doesFileExist file
    if exist then do
      output <- liftIO $ evaluateLisp env =<< readFile file
      outputStrLn output
      loop env 0 ""
    else outputStrLn ("Invalid file name:" <> file) >> loop env count' code
  Just line@(countBrackets count' -> count) -> if
    | count > 0  -> loop env count newCode
    | count == 0 -> do
        output <- liftIO $ evaluateLisp env newCode
        outputStrLn output
        loop env 0 ""
    | otherwise  -> do
        outputStrLn $ printf "You have %d extra brackets in yor code" count
        loop env 0 ""
    where 
      newCode = code <> "\n" <> line
    

prompt :: Int -> String
prompt = printf "[%d]>> "

countBrackets :: Int -> String -> Int
countBrackets count str = let 
  (o,c) = foldl' (\res@(!opened, !closed) -> \case
    '(' -> (opened + 1, closed)
    ')' -> (opened, closed + 1)
    _   -> res
    ) (0, 0) str 
  in count + o - c

evaluateLisp :: Env -> String -> IO String
evaluateLisp env t = parseLisp t & \case
  Right l -> runLisp env (eval l)
  Left err -> pure err


