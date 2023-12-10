import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void
import Data.Either
import Data.Maybe
import Data.List as List
import Data.Bool
import Data.Char
import Data.Map as Map

type Parser = Parsec Void String

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

type LRMap = Map String (String, String)
data Input = Input String LRMap
  deriving Show

lrs = some $ do
      a <- word
      string "= ("
      b <- word
      string ", "
      c <- word
      string ")\n"
      return $ (a, (b, c))

solve2 :: Int -> String -> Input -> Int
solve2 a cur i@(Input lrs maps)
  | final == "ZZZ" = (a+1) * (length lrs)
  | True = solve2 (a+1) final i
  where
    final = List.foldl (\x y -> let lookedup = maps ! x in case y of
                                                                'L' -> fst lookedup
                                                                'R' -> snd lookedup
                                                                _ -> error "direction not L or R"
                       ) cur lrs

solve :: Input -> Int
solve x = solve2 0 "AAA" x

parser :: Parser Input
parser = do
  lr <- word
  string "\n\n"
  lrmap <- Map.fromList <$> lrs
  return $ Input lr lrmap

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
