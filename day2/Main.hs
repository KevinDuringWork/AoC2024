{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

summary :: [Int] -> (Int, Int, Int, Int)
summary a = (sum a, sum m, minimum m, maximum m)
    where m = map abs a

safe :: (Int, Int, Int, Int) -> Bool
safe  (dir, mag_dir, min_step, max_step) =
    abs dir == mag_dir &&
    min_step >= 1 &&
    max_step <= 3

explode :: Int -> [Int] -> [Int]
explode i x = part1 ++ part2
    where
        split = splitAt i x
        part1 = take (i-1) $ fst split
        part2 = snd split

explodeAll :: [Int] -> [[Int]]
explodeAll x = map (`explode` x) [1 .. length x]

main :: IO ()
main = do
    lines <- fmap Text.lines (Text.readFile "input.txt")
    let matrix = fmap (fmap ((read::String->Int) . Text.unpack) . Text.words) lines

    {-- Calculate changes --}
    let diff = fmap (\x -> zipWith (-) (drop 1 x) x ) matrix

    {-- Summarize Changes, i.e, monotonicity, min/max steps --}
    let sum_matrix = map summary diff

    {-- Validate Summary to "Safety" and Count --}
    let safe_list = map safe sum_matrix
    let part1_answer = length (filter id safe_list)

    {-- Part (2): Tolerate a single failure, we must go deeper --}
    let matrix2 = fmap explodeAll matrix
    let diff2 = map (map (\x -> zipWith (-) (drop 1 x) x)) matrix2
    let sum_matrix2 = map (map summary) diff2
    let safe_list2 = map ((not . null) . filter id . map safe) sum_matrix2
    let part2 = zip safe_list safe_list2
    let part2_answer = length (filter id $ map (uncurry (||)) part2)

    print part1_answer
    print part2_answer






