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

move dir pos route m
  | next == Nothing = (False, route)
  | Set.member (dir, pos) route = (True, route)
  | next == Just '#' = move (rotateRight dir) pos route m
  | otherwise = move dir (add pos dir) (Set.insert (dir, pos) route) m
  where
    next = uncurry Matrix.safeGet (add dir pos) m

getSum =
  (+ 1)
    . size
    . Set.map snd

findLoops dir pos mat =
  Set.size
    . Set.fromList
    . Prelude.map (uncurry add)
    . Prelude.filter
      (\(d, p) -> nextElem d p mat == '.' && isLoop dir pos (updateMap (add d p) mat))

nextElem dir pos = uncurry Matrix.getElem (add dir pos)

isLoop dir pos = fst . move dir pos Set.empty

updateMap = Matrix.setElem '#'

day6 :: IO ()
day6 = do
  putStrLn "day6"
  contents <- readFile "input/day6.txt"
  let input = Matrix.fromLists . lines $ contents
  let pos = getStartPos input
  print pos
  let dir = (-1, 0)
  let set = snd . move dir pos Set.empty $ input
  print . getSum $ set
  let route = Set.toList set
  print . findLoops dir pos input $ route
