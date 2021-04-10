
import Data.Char

import Data.List

import Text.Printf

inc' [] = ['a']
inc' (x:xs) | ord x >= ord 'a' && ord x < ord 'z' = chr(ord x + 1) : xs
            | otherwise = 'a' : inc' xs

check1 [x, y] = False
check1 (x:y:z:xs) | ord x + 1 == ord y && ord y + 1 == ord z = True
                  | otherwise = check1 (y:z:xs)

check2 xs = not $ all (`elem` xs) "iol"

check3 :: Eq a1 => [a1] -> Bool
check3 xs = any (>= 4) groups || sum (map (\x -> fromEnum $ x >= 2) groups) >= 2
    where
        groups = map length (group xs)

inc = reverse.inc'.reverse

part1 s | check1 s && check2 s && check3 s = s
        | otherwise = part1 $ inc s

solve :: [Char] -> IO ()
solve s = do
    let s1 = part1 s
    printf "Part 1: %s\n" s1
    printf "Part 2: %s\n" $ part1 $ inc s1

main = solve "cqjxjnds"