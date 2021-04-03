import Data.Char (ord)
import Data.Bits ((.&.), shift)
import Data.Word (Word8)
import Data.ByteString (pack, unpack)
import Crypto.Hash.MD5 as MD5 (init, update, finalize)
import Text.Printf

toWord8 = map (\x -> fromIntegral (ord x) :: Word8)

hash s = unpack $ finalize $ update MD5.init (pack $ toWord8 s)

bytesToWords = foldr (\ x -> (++) [(x .&. 0xF0) `shift` (-4), x .&. 0x0F]) []

zeros = length . takeWhile (== 0)

combo = zeros.bytesToWords.hash

find :: String -> Int -> Int -> Int
find s i c | combo (s ++ show i) == c = i
           | otherwise = find s (i+1) c

part1 :: String -> Int
part1 codeword = find codeword 0 5

part2 :: String -> Int
part2 codeword = find codeword 0 6

solve :: String -> IO ()
solve codeword = do
    printf "Part 1: %d\n" $ part1 codeword
    printf "Part 2: %d\n" $ part2 codeword

main = solve "ckczppom"