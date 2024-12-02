{-# LANGUAGE GHC2021 #-}

module Day1 (day1) where

import Data.Char (isDigit)
import Data.List (sort)

firstNumber :: [[Char]] -> [Int]
firstNumber = map $ read @Int . takeWhile isDigit

lastNumber :: [[Char]] -> [Int]
lastNumber = map $ read @Int . dropWhile isDigit

distance :: (Num b) => [b] -> [b] -> [b]
distance a b = map abs $ zipWith (-) a b

day1a :: [[Char]] -> Int
day1a li = sum $ distance (sort $ firstNumber li) (sort $ lastNumber li)

getSame :: (Eq a) => [a] -> [a] -> [Int]
getSame a b = map (timesInList b) a

timesInList :: (Eq a) => [a] -> a -> Int
timesInList li a = length $ filter (== a) li

day1b :: [[Char]] -> Int
day1b li = sum $ zipWith (*) (getSame (firstNumber li) (lastNumber li)) (firstNumber li)

day1 :: IO ()
day1 = do
  putStrLn "day1"
  contents <- readFile "input/day1.txt"
  let input = lines contents
  print $ day1a input
  print $ day1b input
