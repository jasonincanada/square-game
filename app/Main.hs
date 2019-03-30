module Main where

import Control.Applicative (many, (<|>))
import Control.Monad       (guard, replicateM)
import Data.Char           (digitToInt, isDigit)
import Data.List           (sort)
import Data.Maybe          (maybe)
import NanoParsec          (Parser(..), char, digit, number, run, space, string)
import qualified Data.Map as M


-- I've manually copied just the first solution from Munklar's gigantic file
-- of all solutions, to test parsing of an individual solution into our format
main :: IO ()
main = do
  file <- readFile "solution1.txt"
  let solutions = run parseFile file
  let map       = squareToMap $ snd $ head solutions
  let bs        = blocks map
  mapM_ (putStrLn . show) bs

type Digit    = Int
type Square   = [[Digit]]
type Solution = (Int, Square)

cell :: Parser Digit
cell = digitToInt <$> (digit <* space)

rowOfDigits :: Parser [Digit]
rowOfDigits = replicateM 36 cell <* nl

square :: Parser Square
square = replicateM 36 rowOfDigits

floatingNumber, commaNumber :: Parser String
floatingNumber = many (digit <|> char '.')
commaNumber    = many (digit <|> char ',')

solution :: Parser Solution
solution = do
  string "Solution #"
  id <- number
  string " is Configuration #" >> commaNumber
  string " found in " >> floatingNumber
  string " minutes" >> nl
  s <- square
  return (id, s)

parseFile :: Parser [Solution]
parseFile = do
  string "Solution for N = 8" >> nl >> nl
  squares <- many (solution <* nl)
  string "Tried " >> commaNumber
  string " configurations in " >> floatingNumber
  string " minutes" >> nl
  return squares

nl = char '\n'


type Pos       = (Int, Int)
type SquareMap = M.Map Pos Digit
type Block     = (Pos, Digit)

squareToMap :: Square -> M.Map Pos Digit
squareToMap square = M.fromList list
  where
    list   = zip coords (concat square)
    coords = [ (i `div` side, i `mod` side) | i <- [0..side*side-1] ]
    side   = 36

-- Recursively get all the blocks from the square map
blocks :: SquareMap -> [Block]
blocks smap = case nextBlock smap of
                Nothing             -> []
                Just (block, smap') -> block : blocks smap'

nextBlock :: SquareMap -> Maybe (Block, SquareMap)
nextBlock smap 
  | M.size smap == 0 = Nothing
  | otherwise        = Just (block, smap')
  where
    block      = (coord, digit)

    -- Keys are returned in order already, so just pick the first
    coord      = head $ M.keys smap

    -- Get the digit at the coordinate position
    digit      = smap M.! coord

    -- These are all the individual cells in the block
    swath      = M.fromList [ ((row+r, col+c), digit) | r <- [0..digit-1],
                                                        c <- [0..digit-1] ]
    (row, col) = coord

    -- The new map has these cells removed (so we don't keep picking the same block)
    smap'      = smap M.\\ swath

{-
  λ> main
  ((0,0),7)
  ((0,7),7)
  ((0,14),7)
  ((0,21),7)
  ((0,28),8)
  ((7,0),8)
  ((7,8),8)
  ((7,16),6)
  ((7,22),6)
  ((8,28),8)
  ((13,16),4)
  ((13,20),8)
  ((15,0),5)
  ((15,5),5)
  ((15,10),6)
  ((16,28),8)
  ((17,16),4)
  ((20,0),3)
  ((20,3),6)
  ((20,9),1)
  ((21,9),8)
  ((21,17),8)
  ((21,25),3)
  ((23,0),3)
  ((24,25),5)
  ((24,30),6)
  ((26,0),4)
  ((26,4),5)
  ((29,9),7)
  ((29,16),7)
  ((29,23),7)
  ((30,0),4)
  ((30,30),6)
  ((31,4),5)
  ((34,0),2)
  ((34,2),2)
-}
