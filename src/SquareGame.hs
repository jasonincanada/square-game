{-# Language LambdaCase #-}

module SquareGame where

import qualified Data.Map as M
import qualified Data.Set as S


{--- Types ---}


-- I'll likely optimize how the veiled/unveiled squares are tracked but for now this is
-- conceptually easy to understand
data Board   = Board { squares  :: S.Set Square   -- All squares on this board
                     , unveiled :: S.Set Square   -- Fully unshrouded squares
                     , veiled   :: S.Set Square   -- At least one cell still shrouded
                     } deriving (Show)

type Square  = (Row, Col, Size)
type Row     = Int
type Col     = Int
type Size    = Int

data SquareSide = STop | SRight | SBottom | SLeft
                  deriving (Show)


-- Cells are the smallest geometric unit on our board, with an edge size of 1. Each tile has 4
-- cells. We need to split up a square's tiles because when clicking on a square's side, the
-- full length of the other side of the edge isn't revealed since we don't want to identify
-- whether there's a square side coincident with the current square (other than the side being
-- clicked obviously)
type Cell     = (CellRow, CellCol, CellBorder)
type CellRow  = Int
type CellCol  = Int
type Shrouded = Bool

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
borderCells :: Square -> SquareSide -> [(CellRow, CellCol)]
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
-- side but not the very ends, this keeps any incident edges shrouded
click :: Square -> SquareSide -> [Cell]
click (row, col, size) = \case
  STop    -> [ (r-1,  c+i,  CBottom) | i <- [1..int] ]
  SBottom -> [ (r+s2, c+i,  CTop   ) | i <- [1..int] ]
  SLeft   -> [ (r+i,  c-1,  CRight ) | i <- [1..int] ]
  SRight  -> [ (r+i,  c+s2, CLeft  ) | i <- [1..int] ]
  where
    c    = col  * 2
    r    = row  * 2
    s2   = size * 2
    int  = s2 - 2


-- Map a square to its interior cells
cells :: Square -> [Cell]
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

    topleft     = [ (r, c,     CTopLeft)                 ]
    top         = [ (r, c+i,   CTop)     | i <- [1..int] ]
    topright    = [ (r, cz,    CTopRight)                ]

    left        = [ (r+i, c,   CLeft)    | i <- [1..int] ]
    interior    = [ (r+i, c+j, CNone)    | i <- [1..int],
                                           j <- [1..int] ]
    right       = [ (r+i, cz,  CRight)   | i <- [1..int] ]

    bottomright = [ (rz,  cz,  CBottomRight)             ]
    bottom      = [ (rz,  c+i, CBottom)  | i <- [1..int] ]
    bottomleft  = [ (rz,  c,   CBottomLeft)              ]

