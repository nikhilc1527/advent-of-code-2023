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

type Line = (Integer, Integer, Integer)
type Map = [Line]

solve :: [Integer] -> [Map] -> Integer
solve seeds maps = minimum $ map solve_seed seeds
  where
    solve_seed seed = foldl follow_map seed maps
      where
        follow_map seed map = case filter filterer map of
                                [] -> seed
                                ((a, b, c):[]) -> seed + a - b
            where
              filterer (dest, src, range) = src <= seed && src + range > seed

parser :: Parser ([Integer], [Map])
parser = do
  string "seeds: "
  seeds <- some number
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
        return (a, b, c)       
    single '\n'
    return map
  return (seeds, maps)

main :: IO()
main = do
  file_contents <- readFile file_name
  let parsed = either undefined id $ runParser parser "" file_contents
  -- putStrLn $ show $ parsed
  let (seeds, maps) = parsed
  putStrLn $ show $ solve seeds maps

  return ()
