{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.List
import qualified Data.MultiMap as MultiMap
import Data.List (elemIndex)

check :: MultiMap.MultiMap String String -> MultiMap.MultiMap String String -> String -> String -> [String] -> Bool
check a b x c u = forward_check && backward_check
    where
        forward_check  = c `elem` (a MultiMap.! x) || (elemIndex x u > elemIndex c u)
        backward_check = c `elem` (b MultiMap.! x) || (elemIndex x u < elemIndex c u)
        -- backward_check = True 

validate :: MultiMap.MultiMap String String -> MultiMap.MultiMap String String -> String -> [String] -> Bool
validate a b v update = all (\c -> check a b v c update) (Data.List.filter (v /=) update)

validateAll :: MultiMap.MultiMap String String -> MultiMap.MultiMap String String -> [[String]] -> [Bool]
validateAll a b = fmap (\u -> all (\v -> validate a b v u) u)

stitch :: MultiMap.MultiMap String String -> MultiMap.MultiMap String String -> [String] -> [String]
stitch _ _ [] = [] 
stitch a b i = valid : stitch a b (Data.List.filter (valid /= ) i)
    where
        valid = fst $ head $ filter snd $ fmap (\c -> (c, validate a b c (c : Data.List.filter (c /=) i))) i

main :: IO()
main = do
    content <- Text.readFile "input.txt"
    let
        datum = Text.splitOn "\n\n" content
        raw_inst = Text.lines $ head datum

        -- part 1 
        instruct  = MultiMap.fromList ((\s ->  (head s, last s)) . fmap Text.unpack . Text.splitOn "|" <$> raw_inst)
        instruct' = MultiMap.fromList ((\s ->  (last s, head s)) . fmap Text.unpack . Text.splitOn "|" <$> raw_inst)

        updates = fmap (fmap Text.unpack . Text.splitOn ",") $ Text.lines $ last datum
        validated = [fst x | x <- filter snd $ zip updates (validateAll instruct instruct' updates)]
        computed_answer = sum $ fmap (read::String->Int) [x !! (length x `div` 2) | x <- validated]

        -- part 2 
        invalidated = [fst x | x <- filter (\(_,y) -> not y) $ zip updates (validateAll instruct instruct' updates)]
        permuted = fmap (stitch instruct instruct') invalidated
        computed_answer2 = sum $ fmap (read::String->Int) [x !! (length x `div` 2) | x <- permuted]

    print computed_answer
    print computed_answer2