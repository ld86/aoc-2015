import System.IO
import Data.Set (empty, insert, size, union)
import Data.Set.Internal (Set)
import Text.Printf

houses' [] x y s = s
houses' (h:hs) x y s | h == '^' = houses' hs x (y-1) updatedS
                     | h == 'v' = houses' hs x (y+1) updatedS
                     | h == '>' = houses' hs (x+1) y updatedS
                     | h == '<' = houses' hs (x-1) y updatedS
    where
        updatedS = insert (x,y) s

houses xs = size $ houses' xs 0 0 empty

roboHouses xs = size $ houses' evenCommands 0 0 empty `union` houses' oddCommands 0 0 empty
    where
        evenCommands = [b | (a, b) <- zip [0..] xs, even a]
        oddCommands = [b | (a, b) <- zip [0..] xs, odd a]

part1 = houses

part2 = roboHouses

solve filename = do
    c <- readFile filename
    printf "Part 1: %d\n" $ part1 c
    printf "Part 2: %d\n" $ part2 c

main = solve "input.txt"