{-# LANGUAGE BlockArguments #-}
module Day.One where
import Data.Coerce (coerce)
import Text.ParserCombinators.ReadP (ReadP, char, (<++), readP_to_S, manyTill, eof, optional)
import Util (readP, parse)
import Data.Functor (($>))

type DialPosition = Int
type Instruction = Int

instruction :: ReadP Instruction
instruction = (id <$ char 'R') <++ (negate <$ char 'L') <*> readP

rotate :: DialPosition -> Instruction -> DialPosition
rotate x y = (x + y) `mod` 100

part1 :: String -> String
part1 = parse do
    instructions <- manyTill (instruction <* optional (char '\n')) eof
    let cumulative = scanl rotate 50 instructions
    return $ length $ filter (==0) cumulative

rotateCountingDoubled :: (Int, DialPosition) -> Instruction -> (Int, DialPosition)
rotateCountingDoubled (n,x) y = (n + abs ((x + y) `div` 100 + ((x-1) `mod` 100 + y) `div` 100), (x + y) `mod` 100)

part2 :: String -> String
part2 = parse do
    instructions <- manyTill (instruction <* optional (char '\n')) eof 
    return $ (`div` 2) $ fst $ foldl rotateCountingDoubled (0,50) instructions

one :: (String -> String, String -> String)
one = (part1, part2)