import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void
import Data.Either
import Data.List
import Data.Bool
import Data.Char

import GHC.IO.Unsafe

type Parser = Parsec Void String

logx x = unsafePerformIO $ do
  print x
  return x

whitespace :: Parser ()
whitespace = do
  many $ single ' '
  return ()

word :: Parser String
word = do
  whitespace
  res <- some alphaNumChar
  whitespace
  return res
  
number :: Parser Integer
number = do
  whitespace
  res <- read <$> some digitChar
  whitespace
  return res

type Input = (Double, Double)

solve :: Input -> Int
solve (t, d) = high - low + 1
  where
    disc = logx $ sqrt       $ t*t - 4*d
    low  = ceiling $ (t - disc) / 2 + 0.0001
    high = floor   $ (t + disc) / 2 - 0.0001

pegar :: [Int] -> Int
pegar = foldl (\y x -> let d = length $ show x in y * (10 ^ d) + x) 0

parser :: Parser Input
parser = do
  string "Time:"
  ts <- pegar <$> (some $ read <$> word)
  single '\n'
  string "Distance:"
  ds <- pegar <$> (some $ read <$> word)
  single '\n'
  return $ (fromIntegral $ ts, fromIntegral $ ds)

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
