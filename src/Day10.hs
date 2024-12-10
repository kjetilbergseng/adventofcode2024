{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day10 (day10) where

import Data.Matrix as Matirx
import Data.Set qualified as Set
import UtilityFunctions as Util (chunk)

findZeros = filter (/= (-1, -1)) . toList . mapPos (\p x -> if x == 0 then p else (-1, -1))

findTrail 9 _ pos = [Just pos]
findTrail i m pos = concatMap (filter (/= Nothing) . findDir i m pos) [(1, 0), (-1, 0), (0, 1), (0, -1)]

findDir i m pos dir = if continues i m (add pos dir) then findTrail (i + 1) m (add pos dir) else [Nothing]

add (x, y) (x', y') = (x + x', y + y')

continues i m (x, y) = safeGet x y m == Just (i + 1)

day10a input = sum . map (Set.size . Set.fromList . findTrail 0 input)

day10b input = sum . map (length . findTrail 0 input)

day10 :: IO ()
day10 = do
  putStrLn "day10"
  contents <- readFile "input/day10.txt"
  let input = fromLists . map (map (read @Int) . chunk 1) . lines $ contents
  let startPoints = findZeros input
  print . day10a input $ startPoints
  print . day10b input $ startPoints
