{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day5 (day5) where

import Data.Map
import UtilityFunctions (slide)

day5 :: IO ()
day5 = do
  putStrLn "day5"
  contents <- readFile "input/day5.txt"
  let input = lines contents

  print input
