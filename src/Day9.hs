{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day9 (day9) where

import UtilityFunctions as Util (chunk, (#))

getEveryOtherElement [] res = res
getEveryOtherElement li res = getEveryOtherElement (drop 2 li) (res ++ [head li])

getValues [] _ res = res
getValues li i res = getValues (tail li) (i + 1) (res ++ replicate (head li) i)

dropLast n = reverse . drop n . reverse

takeLast n = take n . reverse

fillHoles [] _ _ res = res
fillHoles _ [] values res = res ++ values
fillHoles filled empty values res = fillHoles (tail filled) (tail empty) updatedList (res ++ f ++ b)
  where
    f = take (head filled) values
    b = takeLast (head empty) (drop (head filled) values)
    updatedList = dropLast (head empty) . drop (head filled) $ values

-- fillHolesB [] _ _ resFront resBack = resFront ++ resBack
-- fillHolesB _ [] values resFront resBack = res ++ values ++ resBack
-- fillHolesB filled empty values resFront resBack = fillHoles (tail filled) (tail empty) updatedList (res ++ f ++ b) -- # ("f:" ++ show f ++ " b:" ++ show b ++ " rest" ++ show updatedList)
--  where
--     f = take (head filled) values
--    b = takeLast (head empty) (drop (head filled) values)
--    updatedList = dropLast (head empty) . drop (head filled) $ values
--    updateEmpty =
--    updateValues =

checksum :: [Int] -> Int
checksum li = sum $ zipWith (*) li [0 .. (length li - 1)]

day1a filled empty values = checksum . fillHoles filled empty values $ []

day9 :: IO ()
day9 = do
  putStrLn "day9"
  contents <- readFile "input/day9.txt"
  let input = map (read @Int) . chunk 1 . head . lines $ contents
  let filled = getEveryOtherElement input []
  let empty = getEveryOtherElement (tail input) []
  let values = getValues filled 0 []
  print . day1a filled empty $ values
