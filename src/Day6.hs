{-# LANGUAGE GHC2021 #-}
{-# HLINT ignore "Use isNothing" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day6 (day6) where

import Data.Matrix as Matrix
import Data.Set as Set

rotateRight (a, b) = (b, -a)

getStartPos =
  head
    . Prelude.filter (/= (-1, -1))
    . Matrix.toList
    . Matrix.mapPos (\(i, j) x -> if x == '^' then (i, j) else (-1, -1))

add (x, y) (i, j) = (x + i, y + j)

move dir pos set m
  | next == Nothing = (False, set)
  | Set.member (dir, pos) set = (True, set)
  | next == Just '#' = move (rotateRight dir) pos set m
  | otherwise = move dir (add pos dir) (Set.insert (dir, pos) set) m
  where
    next = uncurry Matrix.safeGet (add dir pos) m

getSum =
  (+ 1)
    . size
    . Set.map snd

findLoops mat =
  Set.size
    . Set.fromList
    . Prelude.map (uncurry add)
    . Prelude.filter
      (\(d, p) -> nextElem d p mat == '.' && isLoop d p (updateMap (add d p) mat))

nextElem dir pos = uncurry Matrix.getElem (add dir pos)

isLoop dir pos = fst . move dir pos Set.empty

updateMap = Matrix.setElem '#'

day6 :: IO ()
day6 = do
  putStrLn "day6"
  contents <- readFile "input/day6.txt"
  let input = Matrix.fromLists . lines $ contents
  let pos = getStartPos input
  let dir = (-1, 0)
  let set = snd . move dir pos Set.empty $ input
  print . getSum $ set
  let positions = Set.toList set
  print . findLoops input $ positions
