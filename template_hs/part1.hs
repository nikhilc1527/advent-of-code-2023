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

number :: Parser Integer
number = do
  whitespace
  res <- read <$> some digitChar
  whitespace
  return res

word :: Parser String
word = do
  whitespace
  res <- some alphaNumChar
  whitespace
  return res

type Input = ()

solve :: Input -> Int
solve input = undefined

parser :: Parser Input
parser = do
  return ()

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
