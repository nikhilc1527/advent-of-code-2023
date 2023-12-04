-- solution: 27845

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void
import Data.Either
import Data.List
import Data.Bool
import Data.Char

import GHC.IO.Unsafe

type Parser = Parsec Void String

file_name :: String
file_name = "input"

solve :: [([Int], [Int])] -> Int
solve [] = 0
solve ((winning, mine):xs) = res
  where
    rest = solve xs
    my_winning = filter (\x -> x `elem` winning) mine
    cur = 2 ^ (length my_winning) `div` 2
    res = rest + cur

whitespace :: Parser ()
whitespace = do
  many $ single ' '
  return ()

digit_parser :: Parser Char
digit_parser = satisfy isDigit

num_parser :: Parser Int
num_parser = do
  whitespace
  res <- read <$> some digitChar
  whitespace
  return res

line_parser :: Parser ([Int], [Int])
line_parser = do
  string "Card "
  num_parser
  string ":"
  winning <- many num_parser
  string "|"
  mine <- many num_parser
  single '\n'
  return (winning, mine)

full_parser :: Parser [([Int], [Int])]
full_parser = some line_parser

main :: IO()
main = do
  file_contents <- readFile file_name
  let out = either undefined id $ runParser full_parser "" file_contents
  let mapped = map (\(winning, mine) -> filter (\x -> x `elem` winning) mine) out
  -- putStrLn $ show out
  putStrLn $ show $ solve out
  -- putStrLn $ show mapped
  return ()
