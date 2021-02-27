module Sudoku where

import Data.Char (digitToInt, isDigit)
import Data.List ((\\), intercalate, nub)
import Data.Maybe (catMaybes, isNothing)

data Cell = Fixed Int |
            OneOf [Int] deriving (Show)

type Row = [Cell]

type Board = [Row]

emptyCell :: Cell
emptyCell = OneOf [1,2..9]

emptyRow :: Row
--emptyRow = [emptyCell]
emptyRow = replicate 9 emptyCell

emptyBoard :: Board
--emptyBoard = [emptyRow]
emptyBoard = replicate 9 emptyRow

prettyBoard :: Board -> String
prettyBoard = intercalate "\n\n" . map prettyRow
  where
    prettyCell (Fixed c) = show c
    prettyCell _ = "."
    prettyRow = intercalate " " . map prettyCell

readBoard :: String -> Maybe Board
readBoard b =
  let mBoard = map readRow . chunksOf 9 $ b
  in
    if length b /= 81 || any isNothing mBoard
    then Nothing
    else Just . catMaybes $ mBoard
  where
    readRow :: String -> Maybe Row
    readRow r =
      let mRow = map readCell r
      in
        if any isNothing mRow
          then Nothing
          else Just . catMaybes $ mRow

    readCell c
      | c == '.'              = Just emptyCell
      | isDigit c && c /= '0' = Just . Fixed . digitToInt $ c
      | otherwise             = Nothing

    chunksOf _ [] = []
    chunksOf n l =
      let first = take n l
          rest = chunksOf n (drop n l)
      in first : rest

pruneOneOfs :: [Cell] -> [Cell]
pruneOneOfs cells = map pruneOneOf cells
  where
   del = [v | Fixed v <- cells]

   pruneOneOf (Fixed v) = Fixed v
   pruneOneOf (OneOf vs) =
      let rem' = vs \\ del
      in
        if length rem' == 1
        then Fixed . head $ rem'
        else OneOf rem'

isFinished :: Board -> Bool
isFinished b = all isRowFinished b
  where isRowFinished r = all isCellFinished r
        isCellFinished (Fixed _)  = True
        isCellFinished _          = False

isValid :: Board -> Bool
isValid b = isFinished b && all isRowValid b
  where isRowValid r = length (nub ([v| Fixed v <- r])) == 9
