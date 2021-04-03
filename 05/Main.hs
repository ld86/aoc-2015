import System.IO
import Data.List
import Text.Printf

vowels s = length [c | c <- s, c `elem` "aeiou"]

twice s = sum [1 | (a, b) <- zipped, a == b]
    where
        zipped = zip (init s) (tail s)

patterns s = sum [1 | (a, b) <- zipped, (a, b) `elem` [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]]
    where
        zipped = zip (init s) (tail s)

doublePair s = length $ filter equal zz
    where
        z = sort $ zip (init s) (tail s)
        zz = zip (init z) (tail z)
        equal ((a, b), (c, d)) = a == c && b == d

equalTriplets s = sum [1 | (a, b, c) <- t, a == b && b == c]
    where
        t = triplets s

goodTriplets s = sum [1 | (a, b, c) <- t, a == c]
    where
        t = triplets s

nice1 s = (vowels s >= 3) && (twice s >= 1) && (patterns s == 0)

nice2 s = (equalTriplets s < doublePair s) && (goodTriplets s >= 1)

triplets [] = []
triplets [a] = []
triplets [a, b] = []
triplets [a, b, c] = [(a, b, c)]
triplets (a:b:c:xs) = (a, b, c) : triplets (b:c:xs)

part1 l = length $ filter id $ map nice1 (lines l)

part2 l = length $ filter id $ map nice2 (lines l)

solve filename = do
    l <- readFile filename
    printf "Part 1: %d\n" $ part1 l
    printf "Part 2: %d\n" $ part2 l

main = solve "input.txt"