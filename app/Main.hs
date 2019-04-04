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

-}

{-# Language DeriveFunctor #-}
{-# Language TupleSections #-}

module Main where

import Data.Bifunctor (bimap)
import Data.Char      (chr)
import Data.List      ((\\), delete, nub)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  let all      = hylo coalgebra algebra (tiles, squares)
  let complete = map fst $ filter ((==side) . snd) $ all
  mapM_ (putStrLn . display) complete

-- Manually set n=6 for now and find the side length
n    = 6
side = n*(n+1) `div` 2

-- Types
type Tile   = (Int, Int)            -- Row/column of a 1x1 cell on the game board
type Square = Int                   -- Represent a square by its side length
type Board  = M.Map Tile Square     -- A list of placed squares, keyed on the row/col
                                    -- of their upper left corner

-- The collection of starting squares (not a set--we have many squares of the same size)
squares :: [Square]
squares = reverse $ concat $ map (\i -> replicate i i) [1,2 .. n]

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
