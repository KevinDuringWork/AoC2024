{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.List
import qualified Data.MultiMap as MultiMap
import Data.List (elemIndex)

check :: MultiMap.MultiMap String String -> String -> String -> [String] -> Bool
check a x c u = c `elem` (a MultiMap.! x) || (elemIndex x u > elemIndex c u)

validate :: MultiMap.MultiMap String String -> String -> [String] -> Bool
validate a v update = all (\c -> check a v c update) (Data.List.filter (v /=) update)

validateAll :: MultiMap.MultiMap String String -> [[String]] -> [Bool]
validateAll a = fmap (\u -> all (\v -> validate a v u) u)

stitch :: MultiMap.MultiMap String String -> [String] -> [String]
stitch _ [] = [] 
stitch a i = valid : stitch a (Data.List.filter (valid /= ) i)
    where
    valid = fst $ head $ filter snd $ 
        map (\c -> (c, validate a c (c : Data.List.filter (c /=) i))) i

main :: IO()
main = do
    content <- Text.readFile "input.txt"
    
    let
        datum     = Text.splitOn "\n\n" content
        raw_inst  = Text.lines $ head datum
        instruct  = MultiMap.fromList ((\s ->  (head s, last s)) . fmap Text.unpack . Text.splitOn "|" <$> raw_inst)
        updates   = fmap (fmap Text.unpack . Text.splitOn ",") $ Text.lines $ last datum

        -- part 1 
        validated = [fst x | x <- filter snd $ zip updates (validateAll instruct updates)]
        
        -- part 2 
        invalid   = [fst x | x <- filter (\(_,y) -> not y) $ zip updates (validateAll instruct updates)]
        permuted  = fmap (stitch instruct) invalid

        computed_answer  = sum $ fmap (read::String->Int) [x !! (length x `div` 2) | x <- validated]
        computed_answer2 = sum $ fmap (read::String->Int) [x !! (length x `div` 2) | x <- permuted]

    print computed_answer
    print computed_answer2