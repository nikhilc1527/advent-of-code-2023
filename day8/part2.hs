import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void
import Data.Either
import Data.Maybe
import Data.List as List
import Data.Bool
import Data.Char
import Data.Map as Map

import System.IO.Unsafe

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

-- solve2 :: Int -> [String] -> Input -> Int
-- solve2 a cur i@(Input lrs maps)
--   | works new_curs = (a+1) * (length lrs)
--   | True = solve2 (a+1) new_curs i
--   where
--     folder x y = let lookedup = maps ! x in case y of
--                                               'L' -> fst lookedup
--                                               'R' -> snd lookedup
--                                               _ -> error "direction not L or R"
--     new_curs = List.map (\x -> List.foldl folder x lrs) cur
works :: String -> Bool
works s = (s !! 2) == 'Z'

get_cycle :: String -> Integer -> String -> String -> LRMap -> [Integer]
get_cycle original index cur (lr:lrs) map
  | next == original = []
  | works next = (index+1):rest
  | True = rest
    where
      follow x = case lr of
                 'L' -> fst x
                 'R' -> snd x
      next = follow $ (map ! cur)
      rest = get_cycle original (index+1) next lrs map
    
-- solve :: Input -> Int
-- solve x = solve2 0 inlist x
--   where
--     (Input _ a) = x
--     inlist = List.filter (\(u:v:w:[]) -> w == 'A') $ keys $ a

logx x = unsafePerformIO $ do
  print x
  return x

logxy x y = unsafePerformIO $ do
  print x
  return y

-- does same as lcm
intersects :: [[Integer]] -> Integer
intersects lists
  | (length $ List.filter (/= (head heads)) heads) == 0 = head heads
  | True = intersects $ (List.take minIndex lists) ++ [(tail minList)] ++ (List.drop (minIndex + 1) lists)
  where
    heads = List.map head lists
    minIndex = fst $ minimumBy (\(i1, l1) (i2, l2) -> compare l1 l2) $ zip [0..] $ heads
    minList = lists !! minIndex

-- solve x@(Input lr a) = intersects $ cycles -- theoretically works but lcm is just better
-- solve x@(Input lr a) = heads
solve x@(Input lr a) = List.foldr lcm 1 heads
  where
    inlist = List.filter (\(u:v:w:[]) -> w == 'A') $ keys $ a
    cycles = List.map (\s -> cycle $ get_cycle s 0 s lr a) inlist
    heads = List.map List.head cycles

parser :: Parser Input
parser = do
  lr <- cycle <$> word
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
  input@(Input lr map) <- get_input "input"
  -- print $ input
  print $ solve input

  return ()
