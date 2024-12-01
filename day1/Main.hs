{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text 
import qualified Data.Text.IO as Text
import qualified Data.List 

main = do 

   {-- Part 1 [Distance] --}   
  lines <- fmap Text.lines (Text.readFile "input.txt")
  let first  = Data.List.sort $ fmap((read::String->Int) . Text.unpack . head . Text.words) lines 
  let second = Data.List.sort $ fmap((read::String->Int) . Text.unpack . last . Text.words) lines
  let distance = sum $ zipWith (curry(\(x, y) -> abs(x - y))) first second

  {-- Part 2 [Similarity] --}
  let similarity = map (\x -> Data.List.length $ Data.List.filter (==x) second) first
  let distance2 = sum $ zipWith (curry(\(x, y) -> x * y)) first similarity

  print distance
  print distance2   
  
