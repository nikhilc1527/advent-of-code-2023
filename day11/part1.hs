import Data.Vector as V
import Prelude as P

solve file_contents = res
  where
    input = V.map (V.fromList) $ V.fromList $ lines file_contents
    horiz_expand = V.concatMap (\x -> if (V.length $ V.filter (== '#') x) == 0 then V.fromList [x, x] else V.fromList [x])
    transpose x = V.fromList $ P.map (\j -> V.fromList $ P.map (\i -> x ! i ! j) [0..(V.length x)-1]) [0..(V.length $ V.head x)-1]
    expanded = transpose $ horiz_expand $ transpose $ horiz_expand input
    vec_tostr = P.concat . P.map (P.++ "\n") . V.toList . V.map (V.toList)
    gals x = P.filter (/= (-1, -1)) $ P.concat $ P.map (\i -> P.map (\j -> (if (x ! i ! j) == '#' then (i, j) else (-1, -1))) [0..((V.length $ V.head x) - 1)]) [0..((V.length x) - 1)]
    gals_l = gals expanded
    pairs = [(a, b) | a <- gals_l, b <- gals_l, a < b]
    res = P.sum $ P.map (\(a, b) -> (abs $ fst a - fst b) + (abs $ snd a - snd b)) pairs

main :: IO()
main = do
  file_contents <- readFile "input"
  let res = solve file_contents
  print $ res
