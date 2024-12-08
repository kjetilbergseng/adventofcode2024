{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}

module Day8 (day8) where

import Data.Map as Map
import Data.Matrix as Matrix
import Data.Set as Set

getAntennas =
  Map.elems
    . Map.delete '.'
    . Map.fromListWith (++)
    . Matrix.toList
    . Matrix.mapPos (\(i, j) x -> (x, [(i, j)]))

getAntiNodesA s li =
  [add i (sub i j) | i <- li, j <- li, i /= j && isInBounds (add i (sub i j)) s]

getAntiNodesB s li =
  [add i (mul n $ sub i j) | i <- li, j <- li, n <- [0 .. fst s], i /= j && isInBounds (add i (mul n $ sub i j)) s]

sub (i, j) (n, m) = (i - n, j - m)

add (i, j) (n, m) = (i + n, j + m)

mul n (i, j) = (i * n, j * n)

isInBounds (r, c) (nr, nc) =
  r <= nr && r > 0 && c <= nc && c > 0

getSize m = (Matrix.nrows m, Matrix.ncols m)

day8 :: IO ()
day8 = do
  putStrLn "day8"
  contents <- readFile "input/day8.txt"
  let m = Matrix.fromLists $ lines contents
  let antennas = getAntennas m
  print $ (Set.size . Set.fromList . Prelude.concatMap (getAntiNodesA (getSize m))) antennas
  print $ (Set.size . Set.fromList . Prelude.concatMap (getAntiNodesB (getSize m))) antennas
