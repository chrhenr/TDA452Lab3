module Sudoku where

import Data.Char (digitToInt, intToDigit)
import Data.List (nub)
import Data.Maybe (isJust, isNothing, listToMaybe)
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
isSudoku (Sudoku rs) = (length rs == 9) && all isSudokuRow rs

-- | isSudokuRow row checks if row is really a valid representation of a sudoku
-- row
isSudokuRow :: Row -> Bool
isSudokuRow row = (length row == 9) && all isSudokuCell row

-- | isSudokuCell row checks if cell is really a valid representation of a sudoku
-- cell
isSudokuCell :: Cell -> Bool
isSudokuCell Nothing  = True
isSudokuCell (Just n) = n `elem` [1..9]

-- * A3
-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rs) = all (all isJust) rs

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . formatSudoku

formatSudoku :: Sudoku -> String
formatSudoku (Sudoku rs) = formatRows rs

formatRows :: [Row] -> String
formatRows = unlines . map formatRow

formatRow :: Row -> String
formatRow = map formatCell

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
cell :: Gen Cell
cell = frequency [(1, gJust), (9, gNothing)]
  where
    gJust = elements $ map Just [1..9]
    gNothing = return Nothing


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

column :: Int -> Sudoku -> Block
column n (Sudoku rs) = [r !! n | r <- rs]

columns :: Sudoku -> [Block]
columns sud = [column n sud | n <- [0..8]]

threeBy :: Int -> Int -> Sudoku -> Block
threeBy r c (Sudoku rs) = [rs !! x !! y | x <- [(3 * r)..(3 * r + 2)],
                                          y <- [(3 * c)..(3 * c + 2)]]

threeBys :: Sudoku -> [Block]
threeBys sud = [threeBy r c sud | r <- [0..2], c <- [0..2]]

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

-- Returns the list of positions which are blank in the Sudoku
blanks :: Sudoku -> [Pos]
blanks (Sudoku rs) = [(x, y) | x <- [0..8], y <- [0..8], isNothing $ rs !! x !! y]

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = blanks allBlankSudoku == [(x, y) | x <- [0..8], y <- [0..8]]


-- * E2

-- Replaces the element in the provided list with the provided new element at the provided index
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _                     = []
(x:xs) !!= (i,y) | i < 0     = x:xs
                 | i == 0    = y:xs
                 | otherwise = x:(xs !!= (i - 1, y))

prop_bangBangEquals_correct :: [String] -> (Int,String) -> Bool
prop_bangBangEquals_correct xs (i, x') | i >= length xs = xs == (xs !!= (i, x'))
                                       | i < 0          = xs == (xs !!= (i, x'))
                                       | otherwise      = (xs !!= (i, x')) !! i == x'

-- * E3

-- Returns a new Sudoku where cell at the provided position is replaced with the value of the provided cell
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rs) (x, y) c = Sudoku $ rs !!= (x, rowx')
  where
    rowx  = rs !! x
    rowx' = rowx !!= (y, c)

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated s (x, y) c = rs !! x' !! y' == c
  where
    (x', y') = (abs x `mod` 9, abs y `mod` 9)
    (Sudoku rs) = update s (x', y') c

------------------------------------------------------------------------------

-- * F1
-- Solves a sudoku, returns nothing if it isn't solvable
solve :: Sudoku -> Maybe Sudoku
solve sud = listToMaybe $ solve' sud $ blanks sud
  where
    solve' :: Sudoku -> [Pos] -> [Sudoku]
    solve' sud _ | not (isSudoku sud && isOkay sud) = []
    solve' sud []     = [sud]
    solve' sud (b:bs) = concatMap (`solve'` bs) [update sud b c | c <- map Just [1..9]]

-- * F2

-- Reads a file representing a sudoku and prints the solution (or "("no solution")" if none exists)
readAndSolve :: FilePath -> IO ()
readAndSolve fp = do
  sud <- readSudoku fp
  case solve sud of
    Just solution -> printSudoku solution
    Nothing -> putStrLn "(no solution)"

-- * F3

-- Whether the first provided sudoku is a valid solution of the second
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf (Sudoku sol) (Sudoku orig) = isSudoku (Sudoku sol) &&
                                          isOkay (Sudoku sol) &&
                                          null (blanks $ Sudoku sol) &&
                                          all (\(x, y, c) -> sol !! x !! y == c) origCells
  where origCells = [(x, y, orig !! x !! y) |
                                              x <- [0 .. 8],
                                              y <- [0 .. 8],
                                              isJust $ orig !! x !! y]

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound orig = property $ case solve orig of
  Just sol -> sol  `isSolutionOf` orig
  Nothing -> True
