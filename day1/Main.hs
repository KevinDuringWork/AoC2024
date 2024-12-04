{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.List

main :: IO ()
main = do

  
  lines <- fmap Text.lines (Text.readFile "input.txt")
  let
    {-- Part 1 [Distance] --}
    first  = Data.List.sort $ fmap ((read::String->Int) . Text.unpack . head . Text.words) lines
    second = Data.List.sort $ fmap ((read::String->Int) . Text.unpack . last . Text.words) lines
    distance = sum $ zipWith (\ x y -> abs (x - y)) first second

    {-- Part 2 [Similarity] --}
    similarity = map (\x -> Data.List.length $ Data.List.filter (==x) second) first
    distance2 = sum $ zipWith (*) first similarity

  print distance
  print distance2