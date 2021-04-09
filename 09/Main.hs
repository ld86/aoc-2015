import Text.Printf

import Data.List
import qualified Data.Map as M


parseLine = unpack . words
    where
        unpack [from, "to", to, "=", cost] = [((to, from), read cost :: Int), ((from, to), read cost :: Int)]

compute costs route = sum (map (\x -> M.findWithDefault (error "Key") x costs) pairs)
    where
        pairs = zip route (tail route)

part1 l = minimum $ map (compute costs) (permutations $ cities)
    where
        costs = M.fromList $ concatMap parseLine l
        cities = nub.sort $ map fst (M.keys costs)

part2 l = maximum $ map (compute costs) (permutations $ cities)
    where
        costs = M.fromList $ concatMap parseLine l
        cities = nub.sort $ map fst (M.keys costs)



solve filename = do
    l <- lines <$> readFile filename
    printf "Part 1: %d\n" $ part1 l
    printf "Part 2: %d\n" $ part2 l

main = solve "input.txt"