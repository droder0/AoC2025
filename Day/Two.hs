{-# LANGUAGE BlockArguments #-}
module Day.Two where
import Util (readP, parse)
import Text.ParserCombinators.ReadP (ReadP, char, manyTill, eof, sepBy, optional, readP_to_S, many1, get, string)
import Data.List (inits)

part1Invalid :: String -> Bool
part1Invalid s = uncurry (==) $ splitAt (length s `div` 2) s

range :: ReadP [Int]
range = do
    x <- readP @Int
    char '-'
    y <- readP @Int
    return [x .. y]

eitherPart :: (String -> Bool) -> String -> String
eitherPart invalid = parse do
    ranges <- concat <$> sepBy range (char ',')
    optional (char '\n')
    eof
    return $ sum $ filter (invalid . show) ranges

part2Invalid :: String -> Bool
part2Invalid = not . null . readP_to_S do
    s <- many1 get
    many1 $ string s
    eof
    return ()

two :: (String -> String, String -> String)
two = (eitherPart part1Invalid, eitherPart part2Invalid)