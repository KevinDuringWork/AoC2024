{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

parse rule = Parsec.parse rule "(source)"

multP2 :: Parsec.Parsec String() (String, Int, Int)
multP2 = do
    Parsec.string "mul"
    Parsec.char '('
    op1 <- Parsec.many Parsec.digit
    Parsec.char ','
    op2 <- Parsec.many Parsec.digit
    Parsec.char ')'
    return ("DATA", read op1, read op2)

segments2 :: Parsec.ParsecT String () Identity (String, Int, Int)
segments2 = do
    do Parsec.string "\\end"                  >> return ("DATA", 0, 0)
    <|> do Parsec.try (Parsec.string "do()" ) >> return ("DO"  , 0, 0)
    <|> do Parsec.try (Parsec.string "don't") >> return ("DONT", 0, 0)
    <|> Parsec.try multP2
    <|> do Parsec.anyChar >> segments2 

searchMultP2 :: Parsec.ParsecT String () Identity [(String, Int, Int)]
searchMultP2 = Parsec.many $ do segments2

computeScore2 :: String -> [(String, Int, Int)] -> Int
computeScore2 _ [] = 0 
computeScore2 current_state ((state_change, op1, op2):xs) = 
        if | state_change  == "DO"   -> computeScore2 "DO" xs 
           | state_change  == "DONT" -> computeScore2 "DONT" xs         
           | current_state == "DO"   -> (op1*op2) + computeScore2 current_state xs 
           | current_state == "DONT" -> computeScore2 current_state xs 

main :: IO ()
main = do
    contents   <- Text.readFile "example1.txt"
    contents2  <- Text.readFile "example2.txt"

    let
        {-- Part 1 Using Parser Combinators --}
        ans = parse searchMultP2$ Text.unpack contents ++ "\\end"
        Right response = ans
        part1_answer = computeScore2 "DO" response

        {-- Part 2 Incorporating Instructions (Do and Don't) --}
        ans2 = parse searchMultP2 $ Text.unpack contents2 ++ "\\end"
        Right response2 = ans2 
        part2_answer = computeScore2 "DO" response2 

    print part1_answer
    print part2_answer 