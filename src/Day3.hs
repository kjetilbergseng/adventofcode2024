{-# LANGUAGE GHC2021 #-}

module Day3 (day3) where

import Data.Char (isDigit)

calculateSum :: String -> Int
calculateSum [] = 0
calculateSum xs
  | take 4 xs == "mul(" && isDigit (xs !! 4) = parse (drop 4 xs)
  | otherwise = calculateSum $ tail xs

parse :: String -> Int
parse xs = first * second + calculateSum secondTail
  where
    (first, firstTail) = getFirstNumber xs
    (second, secondTail) = getLastNumber firstTail

getFirstNumber :: String -> (Int, String)
getFirstNumber xs = (read @Int $ takeWhile isDigit xs, dropWhile isDigit xs)

getLastNumber :: String -> (Int, String)
getLastNumber [] = (0, [])
getLastNumber [_] = (0, [])
getLastNumber [_, _] = (0, [])
getLastNumber (x : y : ys)
  | x /= ',' = (0, x : y : ys)
  | isDigit y && hasClosingBracket (y : ys) = (read @Int $ takeWhile isDigit (y : ys), tail $ dropWhile isDigit (y : ys))
  | otherwise = (0, y : ys)

hasClosingBracket :: String -> Bool
hasClosingBracket xs
  | null tailEnd = False
  | head tailEnd == ')' = True
  | otherwise = False
  where
    tailEnd = dropWhile isDigit xs

takeUntilDisabled :: [Char] -> [Char] -> [Char]
takeUntilDisabled [] enabled = enabled
takeUntilDisabled (x : xs) enabled
  | take 7 (x : xs) == "don't()" = dropUtilEnabled (drop 6 xs) enabled
  | otherwise = takeUntilDisabled xs (enabled ++ [x])

dropUtilEnabled :: [Char] -> [Char] -> [Char]
dropUtilEnabled [] enabled = enabled
dropUtilEnabled (x : xs) enabled
  | take 4 (x : xs) == "do()" = takeUntilDisabled (drop 3 xs) enabled
  | otherwise = dropUtilEnabled xs enabled

day3 :: IO ()
day3 = do
  putStrLn "day3"
  contents <- readFile "input/day3.txt"
  print $ calculateSum contents
  print $ calculateSum (takeUntilDisabled contents [])
