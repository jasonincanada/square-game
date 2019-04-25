{-# Language LambdaCase      #-}
{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}
{-# Language TupleSections   #-}

module SquareGame where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Arrow  ((>>>))
import           Control.Lens
import           Data.Bifunctor (bimap)
import           System.Random hiding (next)
import           NanoParsec
import           Helpers (filteredKeys, randomIndices)


{--- Types ---}

type Square  = (SRow, SCol, Size)
type SRow    = Int
type SCol    = Int
type Size    = Int
type Tile    = (SRow, SCol)
type TileSet = S.Set Tile

data SquareSide = STop | SRight | SBottom | SLeft
                  deriving (Eq, Ord, Show)


-- Cells are the smallest geometric unit on our board, with an edge size of 1. Each tile has 4
-- cells. We need to split up a square's tiles because when clicking on a square's side, the
-- full length of the other side of the edge isn't revealed since we don't want to identify
-- whether there's a square side coincident with the current square (other than the side being
-- clicked obviously)
type Cell    = (CRow, CCol)
type CRow    = Int
type CCol    = Int
type CellSet = S.Set Cell

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

type SquareMap = M.Map Square (CellSet, CellSet)
type CellGrid  = M.Map Cell (Square, CellBorder)

data Board   = Board { -- Map a square to its shrouded and unshrouded grid cells
                       _squares :: SquareMap

                       -- Map from grid cell to the square it belongs to and the cell's border type
                     , _grid :: CellGrid

                     } deriving (Show)

makeLenses ''Board


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
click :: Square -> SquareSide -> CellSet
click (row, col, size) = \case
  STop    -> S.fromList [ (r-1,  c+i ) | i <- [1..int] ]
  SBottom -> S.fromList [ (r+s2, c+i ) | i <- [1..int] ]
  SLeft   -> S.fromList [ (r+i,  c-1 ) | i <- [1..int] ]
  SRight  -> S.fromList [ (r+i,  c+s2) | i <- [1..int] ]
  where
    c    = col  * 2
    r    = row  * 2
    s2   = size * 2
    int  = s2 - 2


-- Map a square to its tiles
tiles :: Square -> TileSet
tiles (row, col, size) = S.fromList [ (row+r, col+c) | r <- [0..size-1],
                                                       c <- [0..size-1]]

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
next :: SquareSide -> Cell -> Cell
next STop    = bimap id (+1)
next SBottom = bimap id (+1)
next SLeft   = bimap (+1) id
next SRight  = bimap (+1) id


type NextCell = Cell -> Cell

-- Group coordinates into the position and length of their contiguous chunks.  This function
-- assumes the list is already sorted, which is a safe assumption because keys in M.Map and
-- values in S.Set are always sorted.  There is a bit of redundancy here because both takeWhile
-- and drop traverse the position list, so there should be another way to do this that traverses
-- the list only once
contigs :: NextCell -> [Cell] -> [(Cell, Int)]
contigs _    []       = []
contigs next (cell:cs) = (cell, count) : contigs next rest
  where
    count = length $ takeWhile (uncurry (==))
                   $ zip (cell:cs) ray
    ray   = iterate next cell
    rest  = drop (count-1) cs


-- A contiguous run of unshrouded cells along a border implies the square we're traversing is at
-- least this size in the orthogonal direction, so list the coordinates that form a square from
-- this segment of the border
expand :: SquareSide -> (Cell, Int) -> [Cell]
expand side ((row, col), n) = case side of
  STop    -> [ (row+r, col+c) | r <- [1..n-1], c <- [0..n-1]]
  SBottom -> [ (row-r, col+c) | r <- [1..n-1], c <- [0..n-1]]
  SLeft   -> [ (row+r, col+c) | r <- [0..n-1], c <- [1..n-1]]
  SRight  -> [ (row+r, col-c) | r <- [0..n-1], c <- [1..n-1]]


-- Shrouds of width 1 along the board's border can be deshrouded
borderShroud :: CellSet -> CellSet
borderShroud shrouded = S.fromList $ top ++ bottom ++ left ++ right
  where
    -- Hardcoded dimensions of an n=8 Partridge Square
    side   = 36*2
    last   = side-1

    top    = [ (0, col)     | col <- [0..last],
                              (0, col) `S.member`    shrouded,
                              (1, col) `S.notMember` shrouded ]

    bottom = [ (last, col)  | col <- [0..last],
                              (last,   col) `S.member`    shrouded,
                              (last-1, col) `S.notMember` shrouded ]

    left   = [ (row, 0)     | row <- [0..last],
                              (row, 0) `S.member`    shrouded,
                              (row, 1) `S.notMember` shrouded ]

    right  = [ (row, last)  | row <- [0..last],
                              (row, last)   `S.member`    shrouded,
                              (row, last-1) `S.notMember` shrouded ]


-- Initialize a fully shrouded board from a list of Squares
makeBoard :: [Square] -> Board
makeBoard = foldl add (Board M.empty M.empty)
  where
    add :: Board -> Square -> Board
    add Board{..} square = Board squares' grid'
      where
        squares'  = M.insert square (S.fromList positions, S.empty) _squares
        grid'     = M.union (M.fromList cellmap) _grid
        cellmap   = fmap (square,) <$> cs
        cs        = cells square
        positions = map fst cs


-- Deshroud all the tiles in a square
deshroud :: Square -> Board -> Board
deshroud square Board{..} = Board squares' _grid
  where
    squares' = M.adjust f square _squares

    f :: (CellSet, CellSet) -> (CellSet, CellSet)
    f (shrouded, deshrouded) = (S.empty, S.union shrouded deshrouded)

deshroudCells :: CellSet -> Board -> Board
deshroudCells cells board = foldr f board (S.toList cells)
  where
    f :: Cell -> Board -> Board
    f cell (Board squares grid) = let square = fst $ grid M.! cell
                                      squares' = M.adjust (deshroudCell cell) square squares
                                  in  Board squares' grid

    deshroudCell :: Cell -> (CellSet, CellSet) -> (CellSet, CellSet)
    deshroudCell cell (shrouded, unshrouded) = (S.delete cell shrouded, S.insert cell unshrouded)


-- Get the shrouded cells we can unveil from sweeping this edge
sweepEdge :: Square -> SquareSide -> (CellSet, CellSet) -> CellSet
sweepEdge square edge (shrouded, unshrouded) = unveilable
  where
    border       = S.fromList $ borderCells square edge
    int          = S.intersection border unshrouded

    slats        = contigs (next edge) (S.toList int) :: [(Cell, Int)]
    targets      = concatMap (expand edge) slats      :: [Cell]

    unveilable   = S.intersection (S.fromList targets) shrouded


-- If an edge has 14 or more unshrouded cells, it must be an 8-square
sweepableEight :: Square -> (CellSet, CellSet) -> SquareSide -> Bool
sweepableEight square (_, unshrouded) edge = unveilable
  where
    border       = S.fromList $ borderCells square edge
    int          = S.intersection border unshrouded
    unveilable   = S.size int >= 2*n - 2
    n            = 8


-- Filter to fully unshrouded squares
fullSquares :: M.Map Square (CellSet, CellSet) -> [Square]
fullSquares = M.filter ((==S.empty) . fst)
              >>> M.keys


type Seed = Int

-- Using a deterministic RNG with a provided seed, select a random one of each of the square sizes
-- 2,3,..8 and deshroud them ahead of time to start the player off with a partially-revealed board.
-- The same seed will generate the same deshrouding indices every time.  We do this on purpose so
-- leaderboards can accrue records under the same starting conditions.
randomDeshroud :: Seed -> Board -> Board
randomDeshroud seed board = board'
  where
    board'  = fst $ foldl f (board, 2) indices
    indices = take 7 $ randomIndices gen 2
    gen     = mkStdGen seed

    f :: (Board, Int) -> Int -> (Board, Int)
    f (board, size) i = let square = getsquares size board !! (i-1)
                        in  (deshroud square board, size+1)

    -- Get all squares of size n from a board
    getsquares :: Int -> Board -> [Square]
    getsquares size board = filteredKeys p (board ^. squares)
      where
        p (_, _, s) = s == size


clampSquare :: Square -> Square
clampSquare (row, col, size) = (clamp row, clamp col, size)
  where
    clamp i = max 0 (if i + size >= 36
                     then 36-size
                     else i)


{--- IO operations ---}

-- Construct a Board from a squares/*.sqr file
fromFile :: FilePath -> IO Board
fromFile file = readFile file >>= process
  where
    process = lines >>> take 36
                    >>> map (run parseSquare)
                    >>> makeBoard
                    >>> return

parseSquare :: Parser Square
parseSquare = do
  size   <- number <* space
  row    <- number <* char ','
  column <- number
  return (row, column, size)


-- Print an ASCII graphic of the first few lines of this board to the screen
toScreen :: Board -> IO ()
toScreen board = mapM_ printrow [0..20]
  where
    printrow :: Int -> IO ()
    printrow r = mapM_ (printcol r) [0..36*2-1]
                   >> putStrLn ""

    printcol :: Int -> Int -> IO ()
    printcol r c
      | isShrouded (r, c) board = putStr "?"
      | otherwise               = putStr (show $ squareAt (r, c) board)

isShrouded :: Cell -> Board -> Bool
isShrouded cell (Board squares grid) = cell `S.member` shroud
  where
    shroud = fst (squares M.! square)
    square = fst (grid M.! cell)

squareAt :: Cell -> Board -> Int
squareAt cell (Board _ grid) = thd $ fst $ grid M.! cell
  where thd (_, _, x) = x
