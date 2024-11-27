module Sudoku where

import Data.Char (digitToInt, intToDigit)
import Data.List (nub)
import Data.Maybe (isJust)
import Test.QuickCheck

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = (length rs == 9) && (all isSudokuRow rs)

-- | isSudokuRow row checks if row is really a valid representation of a sudoku
-- row
isSudokuRow :: Row -> Bool
isSudokuRow row = (length row == 9) && (all isSudokuCell row)

-- | isSudokuCell row checks if cell is really a valid representation of a sudoku
-- cell
isSudokuCell :: Cell -> Bool
isSudokuCell Nothing  = True
isSudokuCell (Just n) = n `elem` [1..9]

-- * A3
-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rs) = all (\row -> all isJust row) rs

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . formatSudoku

formatSudoku :: Sudoku -> String
formatSudoku (Sudoku rs) = formatRows rs

formatRows :: [Row] -> String
formatRows = unlines . map formatRow

formatRow :: Row -> String
formatRow row = map formatCell row

formatCell :: Cell -> Char
formatCell Nothing  = '.'
formatCell (Just n) = intToDigit n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
  text <- readFile fp
  let soduko = parseSudoku text
  if isSudoku soduko
    then return soduko
    else error "Not a Sudoku!"

parseSudoku :: String -> Sudoku
parseSudoku = Sudoku . parseRows

parseRows :: String -> [Row]
parseRows = map parseRow . lines

parseRow :: String -> Row
parseRow = map parseCell

parseCell :: Char -> Cell
parseCell '.' = Nothing
parseCell c   = Just (digitToInt c)

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(1, gJust), (9, gNothing)]
  where
    gJust = elements $ map Just [1..9]
    gNothing = elements [Nothing]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = fmap Sudoku gRows
    where
      gRow = vectorOf 9 cell
      gRows = vectorOf 9 gRow

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

noDupes :: Eq a => [a] -> Bool
noDupes as = as == nub as

isOkayBlock :: Block -> Bool
isOkayBlock = noDupes . filter isJust

-- * D2

columns :: Sudoku -> [Block]
columns sud = [column n sud | n <- [1..9]]
  where
    column :: Int -> Sudoku -> Block
    column n (Sudoku rs) = [r !! (n - 1) | r <- rs]

threeBys :: Sudoku -> [Block]
threeBys sud = [threeBy r c sud | r <- [1..3], c <- [1..3]]
  where
    threeBy :: Int -> Int -> Sudoku -> Block
    threeBy r c (Sudoku rs) = [rs !! x !! y | x <- [(r - 1)..(r + 1)], y <- [(c - 1)..(c + 1)]]

blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ columns sud ++ threeBys sud

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = all (\block -> length block == 9) . blocks

-- * D3

isOkay :: Sudoku -> Bool
isOkay = all (noDupes . filter isJust) . blocks


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
