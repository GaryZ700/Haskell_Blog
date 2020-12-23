module Lib
    ( playGame
    ) where

import Data.List (isInfixOf)
import Data.List (transpose)
import Data.Char (toUpper)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

playGame :: IO ()
playGame = gameLogic [] 0

gameLogic :: [String] -> Integer -> IO ()
gameLogic foundWords score = do
              if (score == 9)
              then putStrLn "Congratualations!! You found all of the words!!\n\n"
              else do
                      displayGame grid score
                      putStrLn "Enter a word in the grid: "
                      word <- getLine

                      if (findWord grid (map toUpper word) && (elem (map toUpper word) wordsList))
                      then if (elem (map toUpper word) foundWords)
                      then do
                              putStrLn "Alreadly Found Word\n\n"
                              gameLogic foundWords score
                      else do
                              putStrLn "Found Word\n\n"
                              gameLogic ((map toUpper word):foundWords) (score + 1)
                      else do
                              putStrLn "Did Not Find Word\n\n"
                              gameLogic foundWords score

displayGame :: [String] -> Integer -> IO ()
displayGame grid score = do
                            displayGrid grid
                            putStrLn ("Score: " ++ (show score) ++ "/9\n\n")

findWord :: [String] -> String -> Bool
findWord grid word = if (findHorizontalWord grid word)
                        then True
                     else if (findVerticalWord grid word)
                        then True
                     else if (findDiagWord grid word)
                        then True
                     else False

findHorizontalWord :: [String] -> String -> Bool
findHorizontalWord grid word = (or (map (isInfixOf word) grid)) || (or (map (isInfixOf word) (map reverse grid)))

findVerticalWord :: [String] -> String -> Bool
findVerticalWord grid word = findHorizontalWord (transpose grid) word

findDiagWord :: [String] -> String -> Bool
findDiagWord grid word = (findVerticalWord (diagToVertical grid 0) word) || (findVerticalWord (diagToVertical (map reverse grid) 0) word)

diagToVertical :: [String] -> Integer -> [String]
diagToVertical (s:grid) spaces = (addSpace s spaces): (diagToVertical grid (spaces + 1))
diagToVertical [] spaces = []

addSpace :: String -> Integer -> String
addSpace s 0 = s
addSpace s spaces = ' ' : (addSpace s (spaces - 1))

displayGrid :: [String] -> IO ()
displayGrid grid = putStrLn $ unlines grid

grid = [ "LABASDFASDFGFDSCBAZE"
            , "CHEMISTRYQWERTYUDFXZ"
            , "ACBVSDAAMSFGVCXZDPSP"
            , "ASDFBHFDGAASDFASECAH"
            , "SELUCELOMATFSRBRVCDY"
            , "GAFWRRTAHRGHABIMXCAS"
            , "GWGECAPSSADFBMXCVBSI"
            , "HYPOTHESISBAELOKJIAC"
            , "NJHGBCTQWXBNPOILAZGS"
            , "OPIJUHTGBATNMLKIJUHA"
            , "ASEGEENTROPYMNBVCFGT"
            ]
wordsList = ["LAB", "CHEMISTRY", "MATH", "EXPERIMENT", "PHYSICS", "MOLECULES", "SPACE", "HYPOTHESIS", "ENTROPY"]
