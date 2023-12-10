import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void
import Data.Maybe
import Data.Either
import Data.List
import Data.Bool
import Data.Char

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


-- High = 1
-- One = 2
-- Two = 3
-- Three = 4
-- Full = 5
-- Four = 6
-- Five = 7

type HandType = Int

data Play = Play String Integer HandType
  deriving (Show, Eq)
type Input = [Play]

newtype Card = Card Char
  deriving Eq
instance Ord Card where
  compare (Card a) (Card b) = compare (prio a) (prio b)
    where
      prio 'J' = 0
      prio '1' = 1
      prio '2' = 2
      prio '3' = 3
      prio '4' = 4
      prio '5' = 5
      prio '6' = 6
      prio '7' = 7
      prio '8' = 8
      prio '9' = 9
      prio 'T' = 10
      prio 'Q' = 11
      prio 'K' = 12
      prio 'A' = 13

instance Ord Play where
  compare (Play h1 _ ht1) (Play h2 _ ht2) =
    case compare ht1 ht2 of
      EQ -> fromJust $ foldl bab Nothing $ zip h1 h2
      x -> x
    where
      bab m (a, b) = maybe def Just m
        where
          def = case compare (Card a) (Card b) of
                  EQ -> Nothing
                  x -> Just x

handtype hand = g jokers $ f without_jokers
  where
    jokers = length $ filter (== 'J') hand
    without_jokers = filter (/= 'J') hand
    f s = sort $ map length $ group $ sort $ s
    g 0 [5] = 7
    g 0 [1, 4] = 6
    g 0 [2, 3] = 5
    g 0 [1, 1, 3] = 4
    g 0 [1, 2, 2] = 3
    g 0 [1, 1, 1, 2] = 2
    g 0 [1, 1, 1, 1, 1] = 1
    g 1 [4] = 7
    g 1 [1, 3] = 6
    g 1 [2, 2] = 5
    g 1 [1, 1, 2] = 4
    g 1 [1, 1, 1, 1] = 2
    g 2 [3] = 7
    g 2 [1, 2] = 6
    g 2 [1, 1, 1] = 4
    g 3 [2] = 7
    g 3 [1, 1] = 6
    g 4 [1] = 7
    g 5 [] = 7

solve input = sum $ map (\(i, Play _ bid _) -> i * bid) $ zip [1..] $ sort input

hand_parser = do
  hand <- word
  bid <- number
  let ht = handtype hand
  return $ Play hand bid ht

parser = some $ const <$> hand_parser <*> (single '\n')

file_name :: String
file_name = "input"

show_input a = concat $ map (((++) "\n") . show) $ a

main :: IO()
main = do
  file_contents <- readFile file_name
  let parsed = runParser parser "" file_contents
  let input = either undefined id parsed
  -- print $ input
  -- putStrLn $ show_input $ sort input
  print $ solve input
  return ()
