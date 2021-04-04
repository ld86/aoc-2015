{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Map as M
import Data.List (sort)
import Text.Printf

data Action = On | Off | Toggle deriving (Show)
data Point = Point Int Int deriving (Show)
data Command = Command Action Point Point deriving (Show)

data State = State {
    getMap :: M.Map (Int, Int) Int
} deriving (Show)

emptyState = State M.empty

apply1 :: State -> Command -> State
apply1 state (Command On (Point a b) (Point c d)) = State newState
    where
        points = [((i, j), 1) | i <- [a..c], j <- [b..d]]
        newState = M.fromList points `M.union` getMap state
apply1 state (Command Off (Point a b) (Point c d)) = State newState
    where
        points = [((i, j), 0) | i <- [a..c], j <- [b..d]]
        newState = M.fromList points `M.union` getMap state
apply1 state (Command Toggle (Point a b) (Point c d)) = State newState
    where
        points = [((i, j), 1) | i <- [a..c], j <- [b..d]]
        newState = M.unionWith (-) (M.fromList points) (getMap state)

apply2 :: State -> Command -> State
apply2 state (Command On (Point a b) (Point c d)) = State newState
    where
        points = [((i, j), 1) | i <- [a..c], j <- [b..d]]
        newState = M.unionWith (+) (M.fromList points) (getMap state)
apply2 state (Command Off (Point a b) (Point c d)) = State newState
    where
        points = [((i, j), 0) | i <- [a..c], j <- [b..d]]
        newState = M.unionWith (\l r -> maximum [r - 1, 0]) (M.fromList points) (getMap state)
apply2 state (Command Toggle (Point a b) (Point c d)) = State newState
    where
        points = [((i, j), 2) | i <- [a..c], j <- [b..d]]
        newState = M.unionWith (+) (M.fromList points) (getMap state)

parsePoint :: String -> Point
parsePoint point = Point a b
    where
        [a, b] = map (read.T.unpack) $ T.splitOn "," (T.pack point)

getAction ("turn":"off":_) = Off
getAction ("turn":"on":_) = On
getAction ("toggle":_) = Toggle

parse line = Command action a b
    where
        tokens = words line
        action = getAction tokens
        points Toggle = (parsePoint $ tokens !! 1, parsePoint $ tokens !! 3)
        points On = (parsePoint $ tokens !! 2, parsePoint $ tokens !! 4)
        points Off = (parsePoint $ tokens !! 2, parsePoint $ tokens !! 4)
        (a, b) = points action

part1 l = length $ filter (==1) $ M.elems $ getMap (foldl apply1 emptyState (map parse l))

part2 l = sum $ M.elems $ getMap (foldl apply2 emptyState (map parse l))

solve filename = do
    l <- lines <$> readFile filename
    printf "Part1: %d\n" $ part1 l
    printf "Part2: %d\n" $ part2 l

main = solve "input.txt"