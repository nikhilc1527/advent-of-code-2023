-- solution: 9496801

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

solve :: [Int] -> [([Int], [Int])] -> [Int]
solve _ [] = []
solve (y:ys) ((winning, mine):xs) = (y:rest)
  where
    my_winning = length $ filter (\x -> x `elem` winning) mine
    new_ys = (map (+y) $ take my_winning ys) ++ (drop my_winning ys)
    rest = solve new_ys xs

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
  let solved = solve (take (length out) $ repeat 1) out
  putStrLn $ show $ sum solved
  return ()
