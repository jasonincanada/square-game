{-# Language LambdaCase      #-}
{-# Language RecordWildCards #-}

module SquareGame where

import Data.Bifunctor (bimap)
import qualified Data.Map as M
import qualified Data.Set as S


{--- Types ---}


data Board   = Board { -- Map a square to its shrouded and unshrouded grid cells
                       squares :: M.Map Square (S.Set Cell, S.Set Cell)

                       -- Map from grid cell to the square it belongs to and the cell's border type
                     , grid :: M.Map Cell (Square, CellBorder)

                     } deriving (Eq, Show)

type Square = (Row, Col, Size)
type Row    = Int
type Col    = Int
type Size   = Int

data SquareSide = STop | SRight | SBottom | SLeft
                  deriving (Show)


-- Cells are the smallest geometric unit on our board, with an edge size of 1. Each tile has 4
-- cells. We need to split up a square's tiles because when clicking on a square's side, the
-- full length of the other side of the edge isn't revealed since we don't want to identify
-- whether there's a square side coincident with the current square (other than the side being
-- clicked obviously)
type Cell = Pos
type Pos  = (Row, Col)

-- A cell has one and only one of these border types
data CellBorder = CTopLeft
                | CTop
                | CTopRight
                | CRight
                | CBottomRight
                | CBottom
                | CBottomLeft
                | CLeft
                | CNone
                deriving (Eq, Show)


{--- Square<->Cell maps ---}

-- Map a square's side to the cells interior to the square that contact that side
borderCells :: Square -> SquareSide -> [Cell]
borderCells (row, col, size) = \case
  STop    -> [ (r,      c+i   ) | i <- [0..int] ]
  SBottom -> [ (r+s2-1, c+i   ) | i <- [0..int] ]
  SLeft   -> [ (r+i,    c     ) | i <- [0..int] ]
  SRight  -> [ (r+i,    c+s2-1) | i <- [0..int] ]
  where
    r   = row  * 2
    c   = col  * 2
    s2  = size * 2
    int = s2 - 1


-- Map a square's side to the cells revealed when clicking that side, which is almost the whole
-- side but not the very ends, this keeps any coincident edges shrouded
click :: Square -> SquareSide -> [Cell]
click (row, col, size) = \case
  STop    -> [ (r-1,  c+i ) | i <- [1..int] ]
  SBottom -> [ (r+s2, c+i ) | i <- [1..int] ]
  SLeft   -> [ (r+i,  c-1 ) | i <- [1..int] ]
  SRight  -> [ (r+i,  c+s2) | i <- [1..int] ]
  where
    c    = col  * 2
    r    = row  * 2
    s2   = size * 2
    int  = s2 - 2


-- Map a square to its interior cells
cells :: Square -> [(Cell, CellBorder)]
cells (row, col, size) = topleft ++ top ++ topright ++
                         left ++ interior ++ right ++
                         bottomleft ++ bottom ++ bottomright
  where
    -- Top-left cell position
    r    = 2 * row
    c    = 2 * col

    -- Bottom and right extents of the cell grid for this square
    rz   = r + int + 1
    cz   = c + int + 1
    int  = 2 * size - 2

    topleft     = [ ((r, c  ),   CTopLeft)                 ]
    top         = [ ((r, c+i),   CTop)     | i <- [1..int] ]
    topright    = [ ((r, cz ),   CTopRight)                ]

    left        = [ ((r+i, c  ), CLeft)    | i <- [1..int] ]
    interior    = [ ((r+i, c+j), CNone)    | i <- [1..int],
                                             j <- [1..int] ]
    right       = [ ((r+i, cz),  CRight)   | i <- [1..int] ]

    bottomright = [ ((rz,  cz ), CBottomRight)             ]
    bottom      = [ ((rz,  c+i), CBottom)  | i <- [1..int] ]
    bottomleft  = [ ((rz,  c  ), CBottomLeft)              ]


-- Get the next coordinate when traversing the cells along a square's side
next :: SquareSide -> Pos -> Pos
next STop    = bimap id (+1)
next SBottom = bimap id (+1)
next SLeft   = bimap (+1) id
next SRight  = bimap (+1) id


type NextPos = Pos -> Pos

-- Group coordinates into the position and length of their contiguous chunks.
-- This function assumes the list is already ordered
contigs :: NextPos -> [Pos] -> [(Pos, Int)]
contigs _    []     = []
contigs next (a:as) = (a, count) : contigs next rest
  where
    count = length $ takeWhile (uncurry (==))
                   $ zip (a:as) ray
    ray   = iterate next a
    rest  = drop (count-1) as


-- Map a position and length returned from contigs to all the positions we can deshroud
expand :: (Pos, Int) -> SquareSide -> [Pos]
expand ((row, col), n) = \case
  STop    -> [ (row+r, col+c) | r <- [0..n-1], c <- [0..n-1]]
  SBottom -> [ (row-r, col+c) | r <- [0..n-1], c <- [0..n-1]]
  SLeft   -> [ (row+r, col+c) | r <- [0..n-1], c <- [0..n-1]]
  SRight  -> [ (row+r, col-c) | r <- [0..n-1], c <- [0..n-1]]


-- Initialize a fully shrouded board from a list of Squares
board :: [Square] -> Board
board = foldl add (Board M.empty M.empty)
  where
    add :: Board -> Square -> Board
    add Board{..} square = Board squares' grid'
      where
        squares'  = M.insert square (S.fromList positions, S.empty) squares
        grid'     = M.union (M.fromList cellmap) grid
        cellmap   = map (\(pos, border) -> (pos, (square, border))) cs
        cs        = cells square
        positions = map fst cs

