module Day01 where

import Data.Binary.Get (isEmpty)
import Data.List (isPrefixOf, tails)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import System.Environment (getArgs)

extractNumbers :: (String -> Maybe Int) -> String -> Int
extractNumbers condition s = a * 10 + b
  where
    t = tails s
    a = head $ mapMaybe condition t
    b = head $ mapMaybe condition $ reverse t

safeReturn :: Maybe Int -> Int
safeReturn = fromMaybe 0

takeFirst :: (a -> Bool) -> [a] -> Maybe a
takeFirst b l = listToMaybe $ dropWhile (not . b) l

part1 :: String -> String
part1 s = show $ sum $ map (extractNumbers matchDigit) $ lines s

matchDigit :: String -> Maybe Int
matchDigit s
  | "0" `isPrefixOf` s = Just 0
  | "1" `isPrefixOf` s = Just 1
  | "2" `isPrefixOf` s = Just 2
  | "3" `isPrefixOf` s = Just 3
  | "4" `isPrefixOf` s = Just 4
  | "5" `isPrefixOf` s = Just 5
  | "6" `isPrefixOf` s = Just 6
  | "7" `isPrefixOf` s = Just 7
  | "8" `isPrefixOf` s = Just 8
  | "9" `isPrefixOf` s = Just 9
  | otherwise = Nothing

matchNumber :: String -> Maybe Int
matchNumber s
  | isJust d = d
  | "zero" `isPrefixOf` s = Just 0
  | "one" `isPrefixOf` s = Just 1
  | "two" `isPrefixOf` s = Just 2
  | "three" `isPrefixOf` s = Just 3
  | "four" `isPrefixOf` s = Just 4
  | "five" `isPrefixOf` s = Just 5
  | "six" `isPrefixOf` s = Just 6
  | "seven" `isPrefixOf` s = Just 7
  | "eight" `isPrefixOf` s = Just 8
  | "nine" `isPrefixOf` s = Just 9
  | otherwise = Nothing
  where
    d = matchDigit s

part2 :: String -> String
part2 s = show $ sum $ map (extractNumbers matchNumber) $ lines s

main :: IO ()
main = do
  fileContents <- readFile "./input.txt"
  putStrLn $ "Part One: " ++ part1 fileContents
  putStrLn $ "Part Two: " ++ part2 fileContents