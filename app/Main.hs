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

import Parser ( parseLisp )


main :: IO ()
main = runInputT defaultSettings (loop 0 "")

loop :: Int -> String -> InputT IO ()
loop  count' code = getInputLine (prompt count') >>= \case
  Nothing -> loop count' code
  Just line@(countBrackets count' -> count) -> if
    | count > 0  -> loop count newCode
    | count == 0 -> do
        outputStrLn $ parseLisp newCode
        loop 0 ""
    | otherwise  -> do
        outputStrLn $ printf "You have %d extra brackets in yor code" count
        loop 0 ""
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
