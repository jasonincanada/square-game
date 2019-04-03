{- Generate all Partridge Squares for any N using a hylomorphism

   Checkpoint 1:  First attempt at the coalgebra, I haven't run this on input yet but it type-checks!

   Revision: We don't need to iterate over all possible tiles, only the upper-left-most: If
             we've tried all square sizes for that tile and didn't find one that fit, that
             position will never be tiled, so the board can't be a Partridge Square

-}

{-# Language DeriveFunctor #-}
{-# Language TupleSections #-}

module Main where

import Data.Char (chr)
import Data.List ((\\), delete, nub)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  print "coalgebra"

-- Manually set n=3 for now and find the side length
n    = 3
side = n*(n+1) `div` 2

-- Types
type Tile   = (Int, Int)            -- Row/column of a 1x1 cell on the game board
type Square = Int                   -- Represent a square by its side length
type Board  = M.Map Tile Square     -- A list of placed squares, keyed on the row/col
                                    -- of their upper left corner

-- The collection of starting squares (not a set--we have many squares of the same size)
squares :: [Square]
squares = concat $ map (\i -> replicate i i) [1,2 .. n]

-- All grid positions on the board
tiles :: [Tile]
tiles = [ (row, col) | row <- [0,1 .. side-1],
                       col <- [0,1 .. side-1]]


-- Our functor, primed for use in recursion schemes.  NodeF is a node in our trie representing
-- the placement of a square at a tile position. The a represents sub-tries where placements
-- continue with one fewer square of this node's square size and the remainder of available tiles
data TrieF a = NodeF [(Tile, Square, a)]
               deriving (Functor)

-- The fixed point of a functor expressed in Haskell's type system
newtype Fix f = Fix { out :: f (Fix f) }

type Coalgebra f a = a -> f a

-- Branch a node into all possible sub-nodes
coalgebra :: Coalgebra TrieF ([Tile], [Square])
coalgebra (tiles, [])      = NodeF []
coalgebra (tiles, squares) = NodeF subs
  where
    tile       = head tiles
    (row, col) = tile

    subs = [ place square tile span | square <- unique squares,

                                      let span = [ (row+r, col+c) | r <- [0..square-1],
                                                                    c <- [0..square-1]],

                                      all (`elem` tiles) span ]

    place :: Square -> Tile -> [Tile] -> (Tile, Square, ([Tile], [Square]))
    place square tile span = (tile, square, (remainingTiles, remainingSquares))
      where
        remainingTiles   = tiles \\ span
        remainingSquares = delete square squares

    unique = Data.List.nub


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
    blanks = M.fromList $ (,0) <$> tiles

    --     :: a -> f a (!)
    expand :: (Tile, Square) -> [(Tile, Square)]
    expand ((row, col), square) = [ ((row+r, col+c), square) | r <- [0..square-1],
                                                               c <- [0..square-1]]

-- Display a board in ASCII
display :: Board -> String
display board = unlines [[ letter (row, col) | col <- [0 .. side-1]]
                                             | row <- [0 .. side-1]]
    where
      letter pos = let tile = tiled M.! pos
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
