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

data Line = Line Integer Integer Integer
  deriving Show
type Map = [Line]
data Range = Range Integer Integer
  deriving (Eq, Show)

instance Ord Range where
  compare (Range x1 x2) (Range y1 y2) = if x1 == y1 then compare x2 y2 else compare x1 y1

map_ranges :: [Range] -> Map -> [Range]
map_ranges ranges lines = concat $ map somefunc ranges
  where
    somefunc :: Range -> [Range]
    somefunc r = let res = foldr somefunc2 ([r], []) lines in fst res ++ snd res
      where
        somefunc2 :: Line -> ([Range], [Range]) -> ([Range], [Range])
        somefunc2 line (unmapped, mapped) = (some1, some2 ++ mapped)
          where
            (some1, some2) = foldr (\(a, b) (c, d) -> (a ++ c, b ++ d)) ([], []) $ map (f line) unmapped

offrange a b off = Range (a+off) (b+off)

f :: Line -> Range -> ([Range], [Range])
f (Line sb se off) (Range x y) =
      if se < x || sb > y then 
        ([Range x y], [])
      else if sb > x && se < y then 
        ([Range x (sb-1), Range (se+1) y], [offrange sb se off])
      else if sb <= x && se < y then
        ([Range (se+1) y], [offrange x se off])
      else if sb > x && se >= y then
        ([Range x (sb-1)], [offrange sb y off])
      else if sb <= x && se >= y then
        ([], [offrange x y off])
      else
        error $ "non exhaustive range intersections"

-- solve :: [Range] -> [Map] -> Int
solve ranges maps = let (Range a b) = head $ sort $ foldl map_ranges ranges maps in a
-- solve ranges [] = ranges
-- solve ranges (cur_map:maps) = solve (func ranges cur_map) maps

parser :: Parser ([Range], [Map])
parser = do
  string "seeds: "
  seeds <- f <$> some number
  single '\n'
  single '\n'
  maps <- some $ do
    map <- do
      someTill (satisfy $ const True) (single '\n')
      some $ do
        a <- number
        b <- number
        c <- number
        single '\n'
        return $ Line a b c
    single '\n'
    return map
  return (seeds, maps)
  where
    f [] = []
    f (a:b:xs) = (Range a $ a+b-1):(f xs)

get_input file_name = do
  file_contents <- readFile file_name
  let parsed = either undefined id $ runParser parser "" file_contents
  let (seeds, maps) = parsed

  -- destination_start, src_start, range => src_begin, src_end, offset
  let maps2 = map (map (\(Line a b c) -> (Line b (b+c-1) (a-b)))) maps
  return $ (seeds, maps2)

main :: IO()
main = do
  (seeds, maps2) <- get_input "input"
  print $ solve seeds maps2

  return ()
