module Main where

import Data.List (nub, intersperse, intercalate)
import Control.Monad (mplus)
import Control.Monad.State
import Control.Applicative

data Token = X | B | O 
           deriving (Show, Eq)

nextPlayer :: Token -> Token
nextPlayer X = O
nextPlayer O = X

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
full = all id . map (all (/=B)) 

determineStatus :: Board -> Either (Maybe Token) ()
determineStatus b | Just w <- winner b = Left (Just w)
                  | full b             = Left Nothing
                  | otherwise          = Right ()

emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 B

type GameState = (Token, Board)

type GameMonad = StateT GameState IO

player :: GameMonad Token
player = fst <$> get

board :: GameMonad Board
board = snd <$> get

modifyBoard :: (Board -> Board) -> GameMonad ()
modifyBoard f = modify $ (\(token, board) -> (token, f board))

modifyPlayer :: (Token -> Token) -> GameMonad ()
modifyPlayer f = modify (\(token, board) -> (f token, board))

render :: GameMonad ()
render = board >>= lift . putStrLn . ppBoard

mainLoop :: GameMonad ()
mainLoop = do
  render
  status <- determineStatus <$> board
  case status of
    Left Nothing -> lift $ putStrLn "Draw!"
    Left (Just w)-> lift $ putStrLn $ "Congratulations to " ++ ppToken w : "!"
    Right () -> do
      [row, column] <- lift . sequence $ replicate 2 (read <$> getLine)
      tokenAtBoard <- peak row column <$> board
      case tokenAtBoard of
        B -> do 
          tokenToPlace <- player
          modifyBoard (write row column tokenToPlace)
          modifyPlayer nextPlayer
          mainLoop
        _ -> (lift $ putStrLn "That spot is taken, try again.") >> mainLoop  
 
data Rose a = a :> [Rose a] deriving (Show)

root :: Rose a -> a
root (a :> _) = a

children :: Rose a -> [Rose a]
children (_ :> cs) = cs

moves :: Token -> Board -> [Board]
moves token b = do
  row <- [0, 1, 2]
  column <- [0, 1, 2]
  guard $ peak row column b == B
  return $ write row column token b

gameTree :: Token -> Board -> Rose Board
gameTree t b | Just _ <- winner b = b :> []
             | otherwise          = b :> map (gameTree (nextPlayer t)) (moves t b) 

minimax :: Token -> Rose Board -> Rose Int
minimax player = go player
  where
    minimum' [n] = n
    minimum' (-1:_) = -1
    minimum' (n:ns) = n `min` minimum' ns
    maximum' [n] = n
    maximum' (1:_) = 1
    maximum' (n:ns) = n `max` maximum' ns
    go _ (b :> []) = case winner b of
      Just w  -> (if w == player then 1 else -1) :> []
      Nothing -> 0 :> []
    go p (b :> bs) = ((if p == player then maximum' else minimum') $ map root children) :> children
      where
        children :: [Rose Int]
        children = map (go (nextPlayer p)) bs

main :: IO ()
main = do
  putStrLn "Welcome to Tic Tac Toe -- You are X!"
  print $ root . minimax X . gameTree X $ emptyBoard
  print $ root $ gameTree X $ emptyBoard 
  evalStateT mainLoop (X, emptyBoard)