{-# LANGUAGE LambdaCase #-}
module Main where
import Day.One (one)
import Day.Two (two)


days :: [(String -> String, String -> String)]
days = [
    one,
    two
    ]

input :: String -> IO String
input i = readFile ("inputs/" ++ i ++ ".txt")

part :: String -> String -> String -> String
part "1" a = fst (days !! (read a - 1))
part "2" a = snd (days !! (read a - 1))
part "both" a = \inp -> let d = days !! (read a - 1) in fst d inp ++ " " ++ snd d inp
part _ a = const "No such part!"

main :: IO ()
main = do
    putStrLn "What day?"
    day <- getLine
    putStrLn "What part?"
    pt <- getLine
    putStrLn "'sample' or 'input'?"
    inp <- getLine >>= \case
        "sample" -> readFile "sample.txt"
        "s" -> readFile "sample.txt"
        "input" -> input day
        "i" -> input day
    putStrLn (part pt day inp)