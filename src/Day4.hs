{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}

module Day4 (day4) where

import Data.Matrix (Matrix, fromLists, getElem, ncols, nrows, safeGet)
import UtilityFunctions (slide)

getRow :: Int -> Int -> Matrix Char -> [Char]
getRow r c m
  | safeGet r c m == Nothing = []
  | otherwise = getElem r c m : getRow r (c + 1) m

getCol :: Int -> Int -> Matrix Char -> [Char]
getCol r c m
  | safeGet r c m == Nothing = []
  | otherwise = getElem r c m : getCol (r + 1) c m

getDiagonalDown :: Int -> Int -> Matrix Char -> [Char]
getDiagonalDown x y m
  | safeGet x y m == Nothing = []
  | otherwise = getElem x y m : getDiagonalDown (x + 1) (y + 1) m

getDiagonalUp :: Int -> Int -> Matrix Char -> [Char]
getDiagonalUp x y m
  | safeGet x y m == Nothing = []
  | otherwise = getElem x y m : getDiagonalUp (x + 1) (y - 1) m

findXmas :: [Char] -> Int
findXmas li = length $ filter id (map (== "XMAS") (slide 4 li))

findSum :: Matrix Char -> Int
findSum m =
  findInRows m id
    + findInRows m reverse
    + findInCols m id
    + findInCols m reverse
    + findDiagonalDownUpper m id
    + findDiagonalDownUpper m reverse
    + findDiagonalUpUpper m id
    + findDiagonalUpUpper m reverse
    + findDiagonalDownLower m id
    + findDiagonalDownLower m reverse
    + findDiagonalUpLower m id
    + findDiagonalUpLower m reverse

findInRows m fn = sum $ map (\x -> findXmas (fn $ getRow x 1 m)) [1 .. nrows m]

findInCols m fn = sum $ map (\x -> findXmas (fn $ getCol 1 x m)) [1 .. ncols m]

findDiagonalDownUpper m fn = sum $ map (\x -> findXmas (fn $ getDiagonalDown x 1 m)) [1 .. ncols m]

findDiagonalDownLower m fn = sum $ map (\x -> findXmas (fn $ getDiagonalDown 1 x m)) [2 .. nrows m]

findDiagonalUpUpper m fn = sum $ map (\x -> findXmas (fn $ getDiagonalUp 1 x m)) [1 .. nrows m - 1]

findDiagonalUpLower m fn = sum $ map (\x -> findXmas (fn $ getDiagonalUp x (nrows m) m)) [1 .. ncols m]

findMasCross :: Matrix Char -> Int -> Int -> Int -> Int
findMasCross m r c res
  | r == nrows m = res
  | c == ncols m = findMasCross m (r + 1) 2 res
  | (diag1 == "MAS" || diag1 == "SAM") && (diag2 == "MAS" || diag2 == "SAM") =
      findMasCross m r (c + 1) (res + 1)
  | otherwise = findMasCross m r (c + 1) res
  where
    diag1 = take 3 (getDiagonalDown (r - 1) (c - 1) m)
    diag2 = take 3 (getDiagonalUp (r - 1) (c + 1) m)

day4 :: IO ()
day4 = do
  putStrLn "day4"
  contents <- readFile "input/day4.txt"
  let input = lines contents
  let m = fromLists input

  print $ findSum m
  print $ findMasCross m 2 2 0
