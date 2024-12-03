{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)
import qualified System.IO.Error as Parsec
import qualified Distribution.Compat.CharParsing as Parser

parse rule = Parsec.parse rule "(source)"

multP :: Parsec.Parsec String() (String, String)
multP = do
    Parsec.string "mul"
    Parsec.char '('
    op1 <- Parsec.many Parsec.digit
    Parsec.char ','
    op2 <- Parsec.many Parsec.digit
    Parsec.char ')'
    return (op1, op2)

{-- Part 1 Using Parser Combinators --}
segments1 :: Parsec.ParsecT String () Identity (String, String)
segments1 = do
    do Parsec.string "\\end" >> return ("0", "0")
    <|> Parsec.try multP
    <|> do Parsec.anyChar >> segments1

searchMultP1 :: Parsec.ParsecT String () Identity [(String, String)]
searchMultP1 = Parsec.many $ do segments1

computeScore1 :: String -> String -> Int
computeScore1 x y = x1 * y1
    where
        x1 = read x
        y1 = read y

{-- Part 2 Incorporating Instructions (Do and Don't) --}
multP2 :: Parsec.Parsec String() (String, String, String)
multP2 = do
    Parsec.string "mul"
    Parsec.char '('
    op1 <- Parsec.many Parsec.digit
    Parsec.char ','
    op2 <- Parsec.many Parsec.digit
    Parsec.char ')'
    return ("DATA", op1, op2)

segments2 :: Parsec.ParsecT String () Identity (String, String, String)
segments2 = do
    do Parsec.string "\\end"                  >> return ("DATA", "0", "0")
    <|> do Parsec.try (Parsec.string "do()" ) >> return ("DO"  , "0", "0")
    <|> do Parsec.try (Parsec.string "don't") >> return ("DONT", "0", "0")
    <|> Parsec.try multP2
    <|> do Parsec.anyChar >> segments2 

searchMultP2 :: Parsec.ParsecT String () Identity [(String, String, String)]
searchMultP2 = Parsec.many $ do segments2

computeScore2 :: String -> [(String, String, String)] -> Int
computeScore2 _ [] = 0 
computeScore2 current_state ((state_change, op1, op2):xs) = 
        if | state_change == "DO"                              -> computeScore2 "DO" xs 
           | state_change == "DONT"                            -> computeScore2 "DONT" xs         
           | state_change == "DATA" && current_state == "DO"   -> (x1*y1) + computeScore2 current_state xs 
           | state_change == "DATA" && current_state == "DONT" -> computeScore2 current_state xs 
    where
        x1 = read op1 
        y1 = read op2 

main :: IO ()
main = do
    contents   <- Text.readFile "example1.txt"
    contents2  <- Text.readFile "example2.txt"

    let
        -- Part (1) 
        ans = parse searchMultP1 $ Text.unpack contents ++ "\\end"
        Right response = ans
        part1_answer = sum $ fmap (uncurry computeScore1) response

        -- Part (2) 
        ans2 = parse searchMultP2 $ Text.unpack contents2 ++ "\\end"
        Right response2 = ans2 
        part2_answer = computeScore2 "DO" response2 
        -- 
    
    print part1_answer
    print part2_answer 