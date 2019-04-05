{- Generate all Partridge Squares for any N using a hylomorphism

   Checkpoint 1: First attempt at the coalgebra, I haven't run this on input yet
                 but it type-checks!

   Checkpoint 2: Algebra added and it appears to work. Setting n=2 we see all possible
                 failed attempts and no successful ones (recall the first complete
                 Partridge Squares start at n=8)

                 λ> main
                 122
                 -22
                 ---

                 221
                 22-
                 ---

   Revision:     I've added another parameter to the algebra accumulator to count the number
                 of placed squares as the boards are being constructed.  The final number needs
                 to be equal to the side length for us to have a complete Partridge Square.  So
                 now we can generate all possible valid squares.  It works for n=1, and it
                 runs through all possibilities for n=5, finding nothing of course, but it
                 takes basically no time.

                 At n=6 it starts noticeably slowing, needing 48 seconds to run through the
                 the whole space.  I enabled profiling and exported a report, which shows the
                 bottleneck at the subs and remainingTiles functions.

   Revision:     For the first pass at optimization, I switched out lists of tiles for IntSets
                 of them, since IntSet is more performant than Haskell's built-in lists.  This
                 gets the run time for checking all of n=6 down to 25 seconds, from 48, not a
                 very good improvement.  However, when I searched for the first solution to n=8,
                 it returned a malformed board (n8-malformed.txt) due to an oversight in the
                 logic of converting tile addresses, which now lets tiles wrap around to the
                 next row of the board.  This looks funny but it gives me some intuition that
                 there may be a better way to think about representing the objects here in a
                 more abstract manner than with individual tiles/rows/columns.


   Checkpoint 3: I've written a new coalgebra that does away with individual tiles and considers
                 the square in terms of columns being placed up to a certain row.  When placing
                 a sub-square, the affected column levels are increased with a simple sum,
                 instead of removing the individual tiles that span the sub-square.  This
                 removes the set difference operation that was bogging down the first coalgebra.
                 This now checks all of n=6 in 3.7 seconds.  It needs ~10 min to run through n=7
                 though, so I'll need to think of further optimizations before running through
                 n=8.  However, it now finds its first complete, valid Partridge Square, in just
                 under 5 minutes.  See n8-complete-1.txt

-}

{-# Language DeriveFunctor #-}
{-# Language TupleSections #-}

module Main where

import Data.Bifunctor (bimap)
import Data.Char      (chr)
import Data.List      ((\\), delete, nub)
import qualified Data.Map.Strict as M
import qualified Data.IntSet     as S

main :: IO ()
main = do
  --let all    = hylo coalgebra  algebra (tiles, squares)
  let all      = hylo coalgebra2 algebra (levels, squares)
  let complete = map fst $ filter ((==side) . snd) $ all
  let complete'= map fst $ filter (const True    ) $ all
  mapM_ (putStrLn . display) [head $ complete]

-- Manually set n=6 for now and find the side length
n    = 8
side = n*(n+1) `div` 2

-- Types
type Tile   = Int                   -- The integer address of a 1x1 cell on the game board
type Tiles  = S.IntSet              -- Store tiles in a set for efficiency
type Square = Int                   -- Represent a square by its side length
type Board  = M.Map Tile Square     -- A list of placed squares, keyed on the index
                                    -- of their upper left corner

-- The collection of starting squares (not a set--we have many squares of the same size)
squares :: [Square]
squares = reverse $ concat $ map (\i -> replicate i i) [1,2 .. n]

-- All grid positions on the board
tiles :: Tiles
tiles = S.fromList [ tileAt (row, col) | row <- [0,1 .. side-1],
                                         col <- [0,1 .. side-1]]


-- Convert to/from the index and row/column representations of a tile
tileAt :: (Int, Int) -> Int
tileAt (row, col) = row * side + col

tileFrom :: Int -> (Int, Int)
tileFrom tile = (tile `div` side, tile `mod` side)


-- Our functor, primed for use in recursion schemes.  NodeF is a node in our trie representing
-- the placement of a square at a tile position. The a represents sub-tries where placements
-- continue with one fewer square of this node's square size and the remainder of available tiles
data TrieF a = NodeF [(Tile, Square, a)]
               deriving (Functor)

-- The fixed point of a functor expressed in Haskell's type system
newtype Fix f = Fix { out :: f (Fix f) }

type Coalgebra f a = a -> f a

-- Branch a node into all possible sub-nodes
coalgebra :: Coalgebra TrieF (Tiles, [Square])
coalgebra (tiles, [])      = NodeF []
coalgebra (tiles, squares) = NodeF subs
  where
    tile       = head $ S.toList tiles
    (row, col) = tileFrom tile

    subs = [ place square tile span | square <- unique squares,

                                      let span = [ tileAt (row+r, col+c) | r <- [0..square-1],
                                                                           c <- [0..square-1]],

                                      all (`S.member` tiles) span ]

    place :: Square -> Tile -> [Tile] -> (Tile, Square, (Tiles, [Square]))
    place square tile span = (tile, square, (remainingTiles, remainingSquares))
      where
        remainingTiles   = tiles \\\ S.fromList span
        remainingSquares = delete square squares

    unique = Data.List.nub
    (\\\)  = S.difference


type Column = Int
type Level  = Int
type Seed   = ([Level], [Square])

levels :: [Level]
levels = replicate side 0 ++ [side]

coalgebra2 :: Coalgebra TrieF Seed
coalgebra2 (_, [])           = NodeF []
coalgebra2 (levels, squares) = NodeF subs
  where
    row    = minimum levels
    column = indexOf row levels
    space  = length $ takeWhile (==row) (drop column levels)

    subs = [ place square | square <- unique squares,
                            square <= space,          -- Fits horizontally
                            square <= side - row      -- Fits vertically
                            ]

    place :: Square -> (Tile, Square, Seed)
    place square = (tile, square, seed)
      where
        tile     = tileAt (row, column)
        seed     = (levels', squares')
        levels'  = addLevels column square levels
        squares' = delete square squares

    addLevels :: Column -> Square -> [Level] -> [Level]
    addLevels column square (l:ls)
      | column > 0 = l : addLevels (column-1) square ls
      | otherwise  = replicate square (l+square) ++ drop (square-1) ls

    indexOf :: Eq a => a -> [a] -> Int
    indexOf x (l:ls)
      | x == l    = 0
      | otherwise = 1 + indexOf x ls

    unique = Data.List.nub


type Algebra f a = f a -> a

algebra :: Algebra TrieF [(Board,Int)]
algebra (NodeF []            ) = [ (M.empty, 0) ]
algebra (NodeF (placement:ps)) = add placement ++ concat (fmap add ps)
  where
    add :: (Tile, Square, [(Board,Int)]) -> [(Board,Int)]
    add (tile, square, boards) = fmap (bimap (M.insert tile square) (+1)) boards

hylo :: Functor f => Coalgebra f a -> Algebra f b -> a -> b
hylo coalg alg = alg . (fmap $ hylo coalg alg) . coalg


-------------------------------
-- Visualization --------------
-------------------------------

-- A Board only contains the list of placed squares and their upper/left corners, so convert it
-- to a map that tracks the square number at each individual tile (0 if nothing placed there)
render :: Board -> M.Map Tile Square
render board = merged
  where
    merged = M.union placed blanks
    placed = M.fromList $ concatMap expand (M.toList board)
    blanks = M.fromList $ (,0) <$> S.toList tiles

    --     :: a -> f a (!)
    expand :: (Tile, Square) -> [(Tile, Square)]
    expand (tile, square) = let (row, col) = tileFrom tile
                            in  [ (tileAt (row+r, col+c), square) | r <- [0..square-1],
                                                                    c <- [0..square-1]]

-- Display a board in ASCII
display :: Board -> String
display board = unlines [[ letter (row, col) | col <- [0 .. side-1]]
                                             | row <- [0 .. side-1]]
    where
      letter pos = let tile = tiled M.! (tileAt pos)
                   in  if tile == 0
                       then '-'
                       else chr (48 + tile)

      tiled      = render board

{-
  λ> putStrLn $ display (M.fromList [((0,0), 3), ((3,3), 2)])
  333---
  333---
  333---
  ---22-
  ---22-
  ------
-}
