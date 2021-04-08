import Text.Printf

difference1 line = length line - count line

difference2 line = length (show line) - length line

count :: Num a => String -> a
count [] = 0
count ('"':xs) = count xs
count ('\\':'\\':xs) = 1 + count xs
count ('\\':'x':_:_:xs) = 1 + count xs
count (_:xs) = 1 + count xs

part1 l = sum $ map difference1 l

part2 l = sum $ map difference2 l

solve filename = do
    l <- lines <$> readFile filename
    printf "Part 1: %d\n" $ part1 l
    printf "Part 2: %d\n" $ part2 l

main = solve "input.txt"