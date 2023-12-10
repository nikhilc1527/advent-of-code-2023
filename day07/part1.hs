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

data HandType = High | One | Two | Three | Full | Four | Five
  deriving (Eq, Show)

instance Ord HandType where
  compare h1 h2 = compare (prio h1) (prio h2)
    where
      prio High = 1
      prio One = 2
      prio Two = 3
      prio Three = 4
      prio Full = 5
      prio Four = 6
      prio Five = 7

data Play = Play String Integer HandType
  deriving (Show, Eq)
type Input = [Play]

newtype Card = Card Char
  deriving Eq
instance Ord Card where
  compare (Card a) (Card b) = compare (prio a) (prio b)
    where
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
      prio 'J' = 11
      prio 'Q' = 12
      prio 'K' = 13
      prio 'A' = 14

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

handtype :: String -> HandType
handtype hand = g $ f hand
  where
    f s = sort $ map length $ group $ sort $ s
    g [5] = Five
    g [1, 4] = Four
    g [2, 3] = Full
    g [1, 1, 3] = Three
    g [1, 2, 2] = Two
    g [1, 1, 1, 2] = One
    g [1, 1, 1, 1, 1] = High
    g _ = error "unknown hand type"

solve :: Input -> Integer
solve input = sum $ map (\(i, Play _ bid _) -> i * bid) $ zip [1..] $ sort input

hand_parser :: Parser Play
hand_parser = do
  hand <- word
  bid <- number
  let ht = handtype hand
  return $ Play hand bid ht

parser :: Parser Input
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
