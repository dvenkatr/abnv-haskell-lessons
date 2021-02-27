module TicTacToe where

data Player = X | O deriving (Show, Eq)
data Cell = Played Player | Empty deriving (Show, Eq) -- to check equality
data Board = Board [Cell] deriving Show

-- all positions
-- [(x,y) | x <- [0..2], y <- [0..2]]

a :: Board
a = Board [
              Played X, Played X, Played X,
              Played X, Played X, Played X,
              Played X, Played X, Played X
              ]

b :: Board
b = Board [
              Played X, Played X, Played X,
              Played X, Played X, Empty,
              Empty, Played X, Played X
              ]

start :: Board
start = Board [
              Played X, Empty, Empty,
              Empty, Empty, Empty,
              Empty, Empty, Empty
              ]

e :: Board
e = Board [
              Empty, Empty, Empty,
              Empty, Empty, Empty,
              Empty, Empty, Empty
              ]

printCell :: Cell -> String
printCell Empty = " "
printCell (Played v) = show v

printBoard :: Board -> IO()
printBoard (Board list) = putStr (
                              printCell (list !! 0) ++ " | " ++ printCell (list !! 1) ++ " | " ++ printCell (list !! 2) ++ " | " ++ "\n" ++
                              printCell (list !! 3) ++ " | " ++ printCell (list !! 4) ++ " | " ++ printCell (list !! 5) ++ " | " ++ "\n" ++
                              printCell (list !! 6) ++ " | " ++ printCell (list !! 7) ++ " | " ++ printCell (list !! 8) ++ " | " ++ "\n"
                              )

isGameOver :: Board -> Bool
isGameOver (Board game) = notElem Empty game || somebodyWon (Board game)

somebodyWon :: Board -> Bool
somebodyWon (Board game)
  | (game !! 0 == game !! 1) && (game !! 1 == game !! 2) && (game !! 0 /= Empty) = True
  | (game !! 3 == game !! 4) && (game !! 4 == game !! 5) && (game !! 3 /= Empty) = True
  | (game !! 6 == game !! 7) && (game !! 7 == game !! 8) && (game !! 6 /= Empty) = True
  | (game !! 0 == game !! 3) && (game !! 3 == game !! 6) && (game !! 0 /= Empty) = True
  | (game !! 1 == game !! 4) && (game !! 4 == game !! 7) && (game !! 1 /= Empty) = True
  | (game !! 2 == game !! 5) && (game !! 5 == game !! 8) && (game !! 2 /= Empty) = True
  | (game !! 0 == game !! 4) && (game !! 4 == game !! 8) && (game !! 0 /= Empty) = True
  | (game !! 2 == game !! 4) && (game !! 4 == game !! 6) && (game !! 2 /= Empty) = True
  | otherwise = False

play :: Board -> Int -> Player -> Board -- board + position + move
play (Board game) pos move
  | pos > 8 = error "This is a 3x3 board, dumbass!"
  | game !! pos /= Empty = error "Someone has already played here, try again!"
  | otherwise = Board ((take pos game) ++ [Played move] ++ drop (pos+1) game)

compPlay :: Board -> Board
compPlay (Board game) -- comp is X
  | isGameOver (Board game) == True = error "Game over! No valid moves"
  -- | somebodyWon (play (Board game) pos X) == True = play (Board game) pos X
  -- | somebodyWon (play (Board game) pos O) == True = play (Board game) (findPos game Empty) X
  | otherwise = play (Board game) (findPos game Empty) X

userPlay :: Board -> Int -> Board
userPlay (Board game) pos -- user is O
  | isGameOver (Board game) == True = error "Game over! No valid moves"
  | otherwise = play (Board game) pos O

findPos list el
  | list == [] = -1
  | head list == el = 0
  | otherwise = 1 + findPos (tail list) el
