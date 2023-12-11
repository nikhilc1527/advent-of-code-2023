import Data.Vector as V
import Prelude as P

-- solve :: String -> Int -> (Vector Int, Vector Int)
solve file n = res
  where
    input = V.map (V.fromList) $ V.fromList $ lines file
    transpose x = V.fromList $ P.map (\j -> V.fromList $ P.map (\i -> x ! i ! j) [0..(V.length x)-1]) [0..(V.length $ V.head x)-1]
    expand l = V.reverse $ V.tail $ V.fromList $ V.foldl (\(i:xs) has_gal -> if has_gal then (i+1):i:xs else (i+n):i:xs) [0] $ V.map (\i -> (V.length $ V.filter (== '#') $ l ! i) > 0) $ V.fromList [0..(V.length l)-1]
    is = expand $ input
    js = expand $ transpose input
    gals x = P.map (\(a, b) -> (is ! a, js ! b)) $ P.filter (/= (-1, -1)) $ P.concat $ P.map (\i -> P.map (\j -> (if (x ! i ! j) == '#' then (i, j) else (-1, -1))) [0..((V.length $ V.head x) - 1)]) [0..((V.length x) - 1)]
    gals_l = gals input
    pairs = [(a, b) | a <- gals_l, b <- gals_l, a < b]
    res = P.sum $ P.map (\(a, b) -> (abs $ fst a - fst b) + (abs $ snd a - snd b)) pairs

main :: IO()
main = do
  file <- readFile "input"
  let res = solve file 1000000
  print $ res
