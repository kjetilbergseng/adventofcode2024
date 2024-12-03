{-# LANGUAGE GHC2021 #-}

module Day2 (day2) where

import UtilityFunctions (split)

day2a :: [[Char]] -> Int
day2a li = length $ filter id (map (and . isSafeA) (parse li))

day2b :: [[Char]] -> Int
day2b li = length $ filter id (map isSafeB (parse li))

parse :: [[Char]] -> [[Int]]
parse = map (map (read @Int) . split ' ')

isSafeB :: [Int] -> Bool
isSafeB li
  | and (isSafeA li) = True
  | and (isSafeA (tail li)) = True
  | otherwise = and $ isSafeA (filterUnSafe li (isSafeA li))

filterUnSafe :: [Int] -> [Bool] -> [Int]
filterUnSafe li liMask = map fst (takeWhile snd tpls) ++ tail (map fst $ dropWhile snd tpls)
  where
    tpls = zip li (True : liMask)

isSafeA :: [Int] -> [Bool]
isSafeA li@(_ : xs) = zipWith (&&) (isSlopeMonotone slopes) (map isSlopeGradientSafe slopes)
  where
    slopes = zipWith (-) xs li

isSlopeGradientSafe :: Int -> Bool
isSlopeGradientSafe gradient
  | gradient < -3 = False
  | gradient > 3 = False
  | gradient == 0 = False
  | otherwise = True

isSlopeMonotone :: [Int] -> [Bool]
isSlopeMonotone gradients = map (isInDirectionOfSlope direction) gradients
  where
    down = length $ filter (== -1) (map signum gradients)
    up = length $ filter (== 1) (map signum gradients)
    direction
      | down > up = -1
      | otherwise = 1

isInDirectionOfSlope :: Int -> Int -> Bool
isInDirectionOfSlope direction element
  | signum element == direction = True
  | otherwise = False

day2 :: IO ()
day2 = do
  putStrLn "day2"
  contents <- readFile "input/day2.txt"
  let input = lines contents
  print $ day2a input
  print $ day2b input
