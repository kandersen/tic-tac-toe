module Main where

import Data.List (nub, intersperse)
import Control.Monad (mplus)

data Field = X | B | O 
           deriving (Show, Eq)

type Row = [Field]

type Board = [Row]

ppField :: Field -> Char
ppField X = 'X'
ppField B = ' '
ppField O = 'O'

ppRow :: Row -> String
ppRow = intersperse '|' . map ppField

horLine :: String
horLine = "-+-+-"

ppBoard :: Board -> String
ppBoard = concat . intersperse "\n" . intersperse horLine . map ppRow

rows, columns, diagonals :: Board -> [Row]
rows = id

columns [] = []
columns [rs] = map (:[]) rs
columns (rs:rss) = zipWith (:) rs (columns rss)

diagonals b = [diagonal b, diagonal . map reverse $ b]
  where
    diagonal = zipWith (flip (!!)) [0..]

winner :: Board -> Maybe Field
winner b = foldl mplus Nothing . map checkRow $ columns b ++ rows b ++ diagonals b
  where
    checkRow :: Row -> Maybe Field
    checkRow r = case nub r of
      [e] | e /= B -> Just e
      _ -> Nothing

modify :: Int -> (a -> a) -> [a] -> [a]
modify ix f = zipWith (\ix' a -> if ix' == ix then f a else a) [0..]

place :: Field -> Int -> Int -> Board -> Board
place piece row column = modify row (modify column (const piece))

emptyBoard = replicate 3 $ replicate 3 B

ex1 = [[X, X, O],
       [X, O, B],
       [O, B, B]]

gameLoop :: IO ()
gameLoop = undefine

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe -- You are X!"
  print $ rows ex1
  print $ columns ex1
  print $ diagonals ex1
  print $ winner ex1
  print $ place X 2 2 ex1
  putStrLn $ ppBoard ex1