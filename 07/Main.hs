import Data.Bits
import Data.Word
import Data.Char
import Debug.Trace
import Text.Printf

import qualified Data.Map as M

data Operator =
    Value String
    | And String String
    | Or String String
    | LShift String Int
    | RShift String Int
    | Not String deriving (Show)

data Gate = Gate Operator String deriving (Show)

data State = State {
    getMemory :: M.Map String Word16,
    getGates :: M.Map String Operator
} deriving (Show)

parseGate line = getGate tokens
    where
        tokens = words line

        getGate [from, "->", to] = Gate (Value from) to
        getGate ["NOT", from, "->", to] = Gate (Not from) to
        getGate [lhs, "RSHIFT", rhs, "->", to] = Gate (RShift lhs (read rhs)) to
        getGate [lhs, "LSHIFT", rhs, "->", to] = Gate (LShift lhs (read rhs)) to
        getGate [lhs, "AND", rhs, "->", to] = Gate (And lhs rhs) to
        getGate [lhs, "OR", rhs, "->", to] = Gate (Or lhs rhs) to

buildState gates = State M.empty (M.fromList operators)
    where
        operators = map (\(Gate operator to) -> (to, operator)) gates

unary (State memory gates) name f = State (M.insert k v memory) gates
    where
        value = M.findWithDefault (error $ "Cannot find name " ++ name) name memory
        (k, v) = f value

binary (State memory gates) l r f = State (M.insert k v memory) gates
    where
        lValue = M.findWithDefault (error $ "Cannot find name " ++ l) l memory
        rValue = M.findWithDefault (error $ "Cannot find name " ++ r) r memory
        (k, v) = f lValue rValue


eval state@(State memory gates) name | M.member name memory = state
                               | otherwise = value operator
                                where
                                    operator = M.lookup name gates
                                    isVariable = all isAlpha

                                    value Nothing = error $ "Cannot find gate " ++ name
                                    value (Just (Value src)) | isVariable src = unary (eval state src) src (\x -> (name, x))
                                                             | otherwise = State (M.insert name (read src) memory) gates

                                    value (Just (Not src)) | isVariable src = unary (eval state src) src (\x -> (name, complement x))
                                                           | otherwise = State (M.insert name (read src) memory) gates

                                    value (Just (RShift src how)) | isVariable src = unary (eval state src) src (\x -> (name, shiftR x how))
                                                                  | otherwise = State (M.insert name (shiftR (read src) how) memory) gates

                                    value (Just (LShift src how)) | isVariable src = unary (eval state src) src (\x -> (name, shiftL x how))
                                                                  | otherwise = State (M.insert name (shiftL (read src) how) memory) gates

                                    value (Just (And l r)) | isVariable l && isVariable r = binary (eval (eval state l) r) l r (\lValue rValue -> (name, lValue .&. rValue))
                                                           | isVariable l = unary (eval state l) l (\x -> (name, x .&. read r))
                                                           | isVariable r = unary (eval state r) r (\x -> (name, x .&. read l))
                                                           | otherwise  = State (M.insert name (read l .&. read r) memory) gates

                                    value (Just (Or l r)) | isVariable l && isVariable r = binary (eval (eval state l) r) l r (\lValue rValue -> (name, lValue .|. rValue))
                                                          | isVariable l = unary (eval state l) l (\x -> (name, x .|. read r))
                                                          | isVariable r = unary (eval state r) r (\x -> (name, x .|. read l))
                                                          | otherwise  = State (M.insert name (read l .|. read r) memory) gates

part1 l = M.findWithDefault (error "Cannot find a") "a" $ getMemory $ eval (state l) "a"
   where
        gates = map parseGate
        state = buildState.gates

solve filename = do
    l <- lines <$> readFile filename
    printf "Part 1: %d\n" $ part1 l

main = solve "input.txt"