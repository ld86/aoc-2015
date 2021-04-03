{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Text (unpack, pack, splitOn, Text)
import Data.List (sort)
import Text.Printf

wrap dims = 2 * (a*b + b*c + a*c) + a*b
    where
        [a, b, c ] = sort dims

ribbon dims = 2 * (a + b) + product dims
    where
        [a, b, c ] = sort dims

parseDims :: Text -> [Int]
parseDims = map (read.unpack) . splitOn "x"

part1 = sum . map (wrap . parseDims) 

part2 = sum . map (ribbon . parseDims)

solve filename = do
    l <- lines <$> readFile filename
    printf "Part 1: %d\n" $ part1 $ pack <$> l
    printf "Part 2: %d\n" $ part2 $ pack <$> l

main = solve "input.txt"