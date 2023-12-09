import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void
import Data.Either
import Data.List
import Data.Bool
import Data.Char
import Data.Maybe

import GHC.IO.Unsafe

type Parser = Parsec Void String

logx x = unsafePerformIO $ do
  print x
  return x

whitespace :: Parser ()
whitespace = do
  many $ single ' '
  return ()

number :: Parser Int
number = do
  negative <- isJust <$> (optional $ single '-')
  num <- read <$> some digitChar
  let res = if negative then -num else num
  return res

data Input = Input [[Int]]
  deriving Show

diffs :: [Int] -> [Int]
diffs (a:b:cs) = (a - b):(diffs $ b:cs)
diffs (x:[]) = []

extend :: [Int] -> [Int]
extend nums
  | (length $ filter (/= 0) nums) == 0 = 0:nums
  | True = let next_row = extend $ diffs nums in (head next_row + head nums):nums

solve_line :: [Int] -> Int
solve_line = head . extend

solve :: Input -> Int
solve (Input input) = sum $ map solve_line input

line_parser :: Parser [Int]
line_parser = number `sepBy` (single ' ')

parser :: Parser Input
parser = Input <$> map reverse <$> (some $ line_parser <* (single '\n'))

get_input :: String -> IO Input
get_input file_name = do
  file_contents <- readFile file_name
  let parsed = runParser parser "" file_contents
  let input = either undefined id parsed
  return $ input

main :: IO()
main = do
  input <- get_input "input"
  -- print $ input
  print $ solve input

  return ()
