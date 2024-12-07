{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isNothing" #-}

module Day6 (day6) where

import Data.Matrix (Matrix, fromLists, mapPos, safeGet, safeSet, toList)

rotateRight :: (Num b) => (b, a) -> (a, b)
rotateRight (a, b) = (b, -a)

getPos :: [Maybe a] -> Maybe a
getPos (Nothing : xs) = getPos xs
getPos [] = Nothing
getPos (x : _) = x

getStartPos :: Matrix Char -> Maybe (Int, Int)
getStartPos m = getPos $ toList $ mapPos (\(i, j) x -> if x == '.' || x == '#' then Nothing else Just (i, j)) m

getDirection :: (Num a, Num b) => Char -> Maybe (a, b)
getDirection '^' = Just (-1, 0)
getDirection '<' = Just (0, -1)
getDirection '>' = Just (0, 1)
getDirection 'v' = Just (1, 0)
getDirection _ = Nothing

getStartDirection :: Maybe (Int, Int) -> Matrix Char -> Maybe (Int, Int)
getStartDirection Nothing _ = Nothing
getStartDirection (Just (i, j)) m = safeGet i j m >>= getDirection

maybeAdd :: (Num a, Num b) => Maybe (a, b) -> (a, b) -> Maybe (a, b)
maybeAdd (Just (x, y)) (i, j) = Just (x + i, y + j)
maybeAdd _ _ = Nothing

move :: Maybe (Int, Int) -> Maybe (Int, Int) -> Matrix Char -> Maybe (Matrix Char)
move _ Nothing _ = Nothing
move Nothing _ _ = Nothing
move dir@(Just (i, j)) pos@(Just (x, y)) m
  | next == Nothing = m'
  | next == Just '#' = m' >>= move (Just dir') nextPos'
  | otherwise = m' >>= move dir nextPos
  where
    next = safeGet (x + i) (y + j) m
    nextPos = dir >>= maybeAdd pos
    dir' = rotateRight (i, j)
    nextPos' = maybeAdd pos dir'
    m' = safeSet '1' (x, y) m

getSum :: Maybe (Matrix Char) -> Int
getSum Nothing = 0
getSum (Just m) = sum $ toList $ mapPos (\_ x -> if x == '1' then 1 else 0) m

day6 :: IO ()
day6 = do
  putStrLn "day6"
  contents <- readFile "input/day6.txt"
  let input = lines contents
  let m = fromLists input
  let pos = getStartPos m
  let dir = getStartDirection pos m

  print m
  print pos
  print dir
  print $ (getSum . move dir pos) m
