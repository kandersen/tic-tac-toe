module Main where

import Data.List (nub, intersperse, intercalate, minimumBy, maximumBy, sortBy)
import Data.Function (on)
import Control.Monad.State
import Control.Applicative
import Control.Arrow (second, first)

data Player = P1 | P2
            deriving (Eq)

data Token = X | B | O 
           deriving (Show, Eq)

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

tokenOf :: Player -> Token
tokenOf P1 = X
tokenOf P2 = O

type Row = [Token]

type Board = [Row]

ppToken :: Token -> Char
ppToken X = 'X'
ppToken B = ' '
ppToken O = 'O'

ppRow :: Row -> String
ppRow = intersperse '|' . map ppToken

horLine :: String
horLine = "-+-+-"

ppBoard :: Board -> String
ppBoard = intercalate "\n" . intersperse horLine . map ppRow

rows, columns, diagonals :: Board -> [Row]
rows = id

columns [] = []
columns [rs] = map (:[]) rs
columns (rs:rss) = zipWith (:) rs (columns rss)

diagonals b = [diagonal b, diagonal . map reverse $ b]
  where
    diagonal = zipWith (flip (!!)) [0..]

winner :: Board -> Maybe Token
winner b = foldl mplus Nothing . map checkRow $ columns b ++ rows b ++ diagonals b
  where
    checkRow :: Row -> Maybe Token
    checkRow r = case nub r of
      [e] | e /= B -> Just e
      _ -> Nothing

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt ix f = zipWith (\ix' a -> if ix' == ix then f a else a) [0..]

peak :: Int -> Int -> Board -> Token
peak row column = (!!column) . (!!row)

write :: Int -> Int -> Token -> Board -> Board
write row column piece = applyAt row (applyAt column (const piece))

full :: Board -> Bool
full = all id . map (notElem B) 

determineStatus :: Board -> Either (Maybe Token) ()
determineStatus b | Just w <- winner b = Left (Just w)
                  | full b             = Left Nothing
                  | otherwise          = Right ()

emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 B

type GameState = (Player, Board)

type GameMonad = StateT GameState IO

player :: GameMonad Player
player = fst <$> get

board :: GameMonad Board
board = snd <$> get

modifyBoard :: (Board -> Board) -> GameMonad ()
modifyBoard f = modify $ second f

endTurn :: GameMonad ()
endTurn = modify $ first nextPlayer

render :: GameMonad ()
render = board >>= lift . putStrLn . ppBoard >> lift (putStrLn "")

mainLoop :: GameMonad ()
mainLoop = do
  render
  status <- determineStatus <$> board
  case status of
    Left Nothing -> lift $ putStrLn "Draw!"
    Left (Just w)-> lift $ putStrLn $ "Congratulations to " ++ ppToken w : "!"
    Right () -> do
      playerInTurn <- player
      (row, column) <- case playerInTurn of
        P1 -> lift $ read <$> getLine
        P2 -> decideMove playerInTurn <$> board
      modifyBoard $ write row column $ tokenOf playerInTurn
      endTurn
      mainLoop

data Rose a = a :> [Rose a] deriving (Show)

root :: Rose a -> a
root (a :> _) = a

children :: Rose a -> [Rose a]
children (_ :> cs) = cs

moves :: Token -> Board -> [(Board, (Int, Int))]
moves token b = do
  row <- [0, 1, 2]
  column <- [0, 1, 2]
  guard $ peak row column b == B
  return (write row column token b, (row, column))

gameTree :: Player -> Board -> Rose Board
gameTree t b | Just _ <- winner b = b :> []
             | otherwise          = b :> map (gameTree (nextPlayer t) . fst) (moves (tokenOf t) b) 

boundedSearch :: Eq a => a -> (a -> a -> a) -> [a] -> a
boundedSearch bound (<+>) = search
  where
    search [] = error "-- boundedSearch: Can't search empty list"
    search [n] = n
    search (n:ns) | n == bound = n
                  | otherwise  = n <+> search ns

minimax :: Player -> Rose Board -> Rose Int
minimax playerInTurn = go playerInTurn
  where
    go _ (b :> []) = case winner b of
      Just w  -> (if w == tokenOf playerInTurn then 1 else -1) :> []
      Nothing -> 0 :> []
    go p (_ :> bs) = (if p == playerInTurn then boundedSearch 1 max else boundedSearch (-1) min) (map root childrenMinimax) :> childrenMinimax
      where
        childrenMinimax :: [Rose Int]
        childrenMinimax = map (go (nextPlayer p)) bs

decideMove :: Player -> Board -> (Int, Int)
decideMove p b = snd . minimumBy (compare `on` (root . fst)) . map (first (minimax p . gameTree p)) $ moves (tokenOf p) b

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe -- You are X!"
  evalStateT mainLoop (P2, emptyBoard)