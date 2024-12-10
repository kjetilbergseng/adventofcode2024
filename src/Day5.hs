{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day5 (day5) where

import Data.Map as Map
import Data.Maybe as Maybe
import UtilityFunctions as Util (split)

getRules =
  Map.fromListWith (++)
    . Prelude.map
      ( (\(x : xs) -> (read @Int x, Prelude.map (read @Int) xs))
          . Util.split '|'
      )
    . takeWhile (/= "")

getValues =
  Prelude.map (Prelude.map (read @Int) . Util.split ',')
    . tail
    . dropWhile (/= "")

isCorrect _ [] = True
isCorrect r vs
  | any (`elem` vs) afterLast = False
  | otherwise = isCorrect r (init vs)
  where
    afterLast = fromMaybe [] (Map.lookup (last vs) r)

correct r vs = Prelude.map (`elem` vs) afterLast
  where
    afterLast = fromMaybe [] (Map.lookup (last vs) r)

middle li = head . Prelude.drop n $ li
  where
    n = div (length li) 2

day5a rules = sum . Prelude.map middle . Prelude.filter (isCorrect rules)

day5 :: IO ()
day5 = do
  putStrLn "day5"
  contents <- readFile "input/day5.txt"
  let input = lines contents
  let rules = getRules input
  let values = getValues input
  print . day5a rules $ values
  print . Prelude.map (correct rules) $ values
