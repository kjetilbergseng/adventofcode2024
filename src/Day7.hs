{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day7 (day7) where

import UtilityFunctions (split)

data Tree a
  = Empty
  | Node a [Tree a]
  deriving (Show, Eq)

parse :: [Char] -> (Int, [String])
parse li = (read @Int $ head input, tail input)
  where
    input = split ' ' $ filter (/= ':') li

createTree :: [String] -> Tree String
createTree [] = Empty
createTree [x] = Node x [Empty]
createTree (x : xs) = Node x [Node "+" [createTree xs], Node "*" [createTree xs], Node "||" [createTree xs]]

concatInt :: Int -> Int -> Int
concatInt a b = read @Int (show a ++ show b)

calculate :: Int -> Tree String -> (Int -> Int -> Int) -> Int -> Int
calculate res (Node "*" n) _ t = calculate res (head n) (*) t
calculate res (Node "+" n) _ t = calculate res (head n) (+) t
calculate res (Node "||" n) _ t = calculate res (head n) concatInt t
calculate res (Node v [Empty]) op currentTotal
  | res == total = res
  where
    total = op currentTotal (read @Int v)
calculate res (Node v n) op currentTotal
  | total > res = 0
  | otherwise = maximum $ map (\x -> calculate res x op total) n
  where
    total = op currentTotal (read @Int v)
calculate _ _ _ _ = 0

day7 :: IO ()
day7 = do
  putStrLn "day7"
  contents <- readFile "input/day7.txt"
  let input = map parse $ lines contents

  print $ sum (map (\x -> calculate (fst x) ((createTree . snd) x) (+) 0) input)
