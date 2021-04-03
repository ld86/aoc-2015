import System.IO
import Text.Printf

basement' [] m i | m < 0 = i
                 | otherwise = -1
basement' (x:xs) m i | m < 0 = i
                     | x == '(' = basement' xs (m+1) (i+1)
                     | x == ')' = basement' xs (m-1) (i+1)

height' [] m = m
height' (x:xs) m | x == '(' = height' xs (m+1)
                 | x == ')' = height' xs (m-1)

part1 :: String -> Int
part1 c = height' c 0 

part2 :: String -> Int
part2 c = basement' c 0 0

solve filename = do
    c <- readFile filename
    printf "Part 1: %d\n" $ part1 c
    printf "Part 2: %d\n" $ part2 c

main = do
    solve "input.txt"