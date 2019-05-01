{- Various functions for populating the database with base game data -}

module SQLUtil where

import qualified Data.Map as M
import           Control.Lens
import           Control.Monad (forM_)
import           Text.Printf
import           SquareGame

type Sequence      = String
type SequenceIndex = Int

-- Write a .sql script to file that can be fed into sqlite3 from the command line
writeScript :: Int -> Int -> IO ()
writeScript from to = do
  all <- getSequences

  forM_ [from..to] $ \idx -> do
    putStrLn $ "Processing " ++ show idx ++ " " ++ (all !! (idx-1))

    script <- boardInsertScript all idx

    let filename = "insert-" ++ show idx ++ ".sql"

    writeFile filename script


boardInsertScript :: [Sequence] -> SequenceIndex -> IO String
boardInsertScript allSequences idx = do

  -- the idx arg is 1-based for clarity so use idx-1
  let sequence  = allSequences !! (idx-1)
  let file = "generation/squares/N8-" ++ sequence ++ ".sqr"

  board <- fromFile file

  let ins = printf "INSERT INTO board (boardID, sequence) VALUES (%d, '%s');" idx sequence

  let squares' = M.keys (board ^. squares)
  let inserts = map (toInsert idx) squares'

  return $ unlines (ins : inserts)


toInsert :: SequenceIndex -> Square -> String
toInsert idx (row, col, size) = printf "INSERT INTO boardSquare (fkBoardID, squareRow, squareCol, squareSize) VALUES (%d, %d, %d, %d);"
                  idx row col size

getSequences :: IO [Sequence]
getSequences = lines <$> readFile "generation/squares/N8-sequences.txt"
