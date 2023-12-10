import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void
import Data.Either
import Data.List
import Data.Bool

import GHC.IO.Unsafe

type Parser = Parsec Void String

file_name :: String
file_name = "input"

data Bolz = Red Int | Blue Int | Green Int
  deriving Show

solve :: Int -> [[Bolz]] -> Int
solve idx [] = 0
solve idx bolz = res
  where
    next            = solve (idx + 1) $ tail bolz
    cur_line        = head bolz
    check (Red x)   = x <= 12
    check (Green x) = x <= 13
    check (Blue x)  = x <= 14
    gud             = and $ map check cur_line
    res             = (bool 0 idx gud) + next

draw_parser :: Parser Bolz
draw_parser = do
  num <- read <$> many digitChar
  single ' '
  color <- (const Red <$> string "red") <|>
              (const Blue <$> string "blue") <|>
              (const Green <$> string "green")
  return $ color num

game_parser :: Parser [Bolz]
game_parser = draw_parser `sepBy` (string ", ")

line_parser :: Parser [Bolz]
line_parser = do
  string "Game "
  many digitChar
  string ": "
  bolzes <- concat <$> game_parser `sepBy` (string "; ")
  single '\n'
  return bolzes

full_parser :: Parser [[Bolz]]
full_parser = some line_parser

main :: IO()
main = do
  file_contents <- readFile file_name
  let out = either undefined id $ runParser full_parser "" file_contents
  putStrLn $ show $ solve 1 out
  return ()
