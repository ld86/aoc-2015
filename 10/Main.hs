import Data.List
import Text.Printf

transform line = concatMap (\x -> show (length x) ++ [head x]) (group line)

part1 initString = length $ last $ take 41 $ iterate transform initString

part2 initString = length $ last $ take 51 $ iterate transform initString

solve :: [Char] -> IO ()
solve initString = do
    printf "Part 1: %d\n" $ part1 initString
    printf "Part 2: %d\n" $ part2 initString

main = solve "1113122113"