{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.List

type Grid           = [[Char]]
type Coordinate     = (Int, Int)
type Direction      = Coordinate
type ExpectedValue  = String
type GridValue      = Char

inGrid :: Grid -> Direction -> Coordinate -> Bool
inGrid grid (y2, x2) (y,x) =
    y+y2 >= 0 && y+y2 < height &&
    x+x2 >= 0 && x+x2 < width
    where
        (height, width) = (length grid, length $ head grid)
         
generateDirections :: Coordinate -> Grid -> [Direction]
generateDirections coordinate grid = filter
    (\(y2,x2) -> inGrid grid (y2, x2) coordinate && (y+y2,x+x2) /= coordinate)
    [(y1, x1) | x1 <- [-1..1], y1 <-[-1..1]]
    where
        (y,x) = coordinate

gridValue :: Grid -> Coordinate -> Char
gridValue grid (y, x) = grid !! y !! x

{-- Part (1) Recursive traversal with a set direction --}
searchCount :: Grid -> Direction -> ExpectedValue -> Coordinate -> Int
searchCount grid direction "" coordinate = 0
searchCount grid direction expectedValue coordinate =
    if gv == ev then (
        if
           | evs == "" -> 1
           | direction == (0,0) -> 
                sum $ fmap (\(y1,x1) -> searchCount grid (y1, x1) evs (y+y1, x+x1)) 
                (generateDirections (y,x) grid)
           | inGrid grid direction coordinate -> searchCount grid direction evs (y+y1, x+x1)
           | otherwise -> 0)
    else 0
    where
        gv       = gridValue grid coordinate
        (y,   x) = coordinate
        (y1, x1) = direction
        (ev:evs) = expectedValue

{-- Part (2) Hardcoding all rotations of X MAS --}
searchCount2 :: Grid -> Coordinate -> Bool
searchCount2 grid coordinate =
    if  | bounds && gv tl == 'M' && gv tr == 'M' && gv bl == 'S' && gv br == 'S' -> True
        | bounds && gv tl == 'S' && gv tr == 'M' && gv bl == 'S' && gv br == 'M' -> True
        | bounds && gv tl == 'S' && gv tr == 'S' && gv bl == 'M' && gv br == 'M' -> True
        | bounds && gv tl == 'M' && gv tr == 'S' && gv bl == 'M' && gv br == 'S' -> True
        | otherwise -> False
    where
        (y, x)    = coordinate
        ig        = inGrid grid (0,0)
        gv        = gridValue grid
        (tl, tr)  = ((y,x),      (y,x+2))
        (bl, br)  = ((y+2,x), (y+2, x+2))
        center    = (y+1, x+1)
        bounds    = (ig tl && ig tr) && 
                    (ig bl && ig br) && 
                    ig center && 
                    gv center == 'A' 

main :: IO()
main = do
    lines   <- fmap Text.lines (Text.readFile "example1.txt")

    let
        letter_map1  = fmap Text.unpack lines
        coor         = [(y, x) | x <- [0..length (head letter_map1) - 1], y <-[0..length letter_map1 - 1]]
        answer1      = map (searchCount letter_map1 (0,0) "XMAS") coor
        answer2      = map (searchCount2 letter_map1) coor

    print $ sum answer1
    print $ length $ filter id answer2