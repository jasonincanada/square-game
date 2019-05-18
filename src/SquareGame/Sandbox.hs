{-# Language DeriveFunctor #-}

{- Sandbox.hs  - Sandbox enviro for naming/addressing all N=8 partridge squares -}

module SquareGame.Sandbox where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S
import           SquareGame

type Height      = Int
type Width       = Int
type Rectangle   = (SRow, SCol, Height, Width)
type Region      = [Rectangle]
type SizeMap     = IM.IntMap Size


--allSquares :: [Size]
--allSquares = concat [ replicate i i | i <- [1..8] ]
allSizes :: SizeMap
allSizes = IM.fromList [ (i, i) | i <- [1..8] ]

topLeftTile, top2x2 :: Region
topLeftTile = [ (0,0,1,1) ]
top2x2      = [ (0,0,2,2) ]

-- Known symmetrically-tileable regions
bow, garden :: Region
garden      = [ (0,0,16,9) ]
bow         = [ (0 ,16,6 ,12)
              , (6 ,0 ,15,28)
              , (28,28,8 ,8 ) ]

tilesFor :: Region -> TileSet
tilesFor areas = foldr S.union S.empty $ for <$> areas
  where
    for (row, col, height, width) = S.fromList [ (row+r, col+c) | r <- [0..height-1],
                                                                  c <- [0..width -1]]

-- Pre-map all possible squares and positions to the sets of tiles they cover
squareTiles :: M.Map Square TileSet
squareTiles = M.fromList [ (square, for square) | square <- squares ]
  where
    squares = [ (row, col, size) | size <- [1..8      ],
                                   row  <- [0..36-size],
                                   col  <- [0..36-size] ]

    for (row, col, size) = S.fromList [ (row+r, col+c) | r <- [0..size-1],
                                                         c <- [0..size-1]]


data TrieF a = NodeF [(Square, a)]
               deriving (Functor)

type Coalgebra f a =   a -> f a

-- Branch a node into all possible sub-nodes
coalgebra :: Coalgebra TrieF (TileSet, SizeMap)
coalgebra (tiles, sizes)
  | tiles == S.empty  = NodeF []
  | sizes == IM.empty = NodeF []
  | otherwise         = NodeF subs
  where
    (row, col) = head $ S.toList tiles

    subs = [ place square | size <- IM.keys sizes,
                                      
                            let square       = (row, col, size),
                            let span         = squareTiles M.! square,
                            let intersection = S.intersection span tiles,

                            S.size intersection == size*size ]

    place :: Square -> (Square, (TileSet, SizeMap))
    place square = (square, (remainingTiles, remainingSquares))
      where
        remainingTiles  = S.difference tiles (squareTiles M.! square)

        -- foldrWithKey :: (Key -> a -> b -> b) -> b -> IntMap a -> b
        remainingSquares = IM.foldrWithKey decide IM.empty sizes

        decide :: Size -> Size -> (SizeMap -> SizeMap)
        decide s count
          | s == size && count == 1 = id
          | s == size && count >  1 = IM.insert s (count-1)
          | otherwise               = IM.insert s count
          where
            (_, _, size) = square



type Algebra f a = f a -> a

algebra :: Algebra TrieF [S.Set Square]
algebra (NodeF []    ) = [S.empty]
algebra (NodeF boards) = concatMap addsquare boards
  where
    addsquare :: (Square, [S.Set Square]) -> [S.Set Square]
    addsquare (square, sets) = map (S.insert square) sets

hylo :: Functor f => Coalgebra f a -> Algebra f b -> a -> b
hylo coalg alg = alg . (fmap $ hylo coalg alg) . coalg

{-
    λ> hylo coalgebra algebra (tilesFor top2x2, allSizes)
    [fromList [(0,0,1)],fromList [(0,0,2)]]

    λ> tilesFor top2x2
    fromList [(0,0),(0,1),(1,0),(1,1)]

    λ> allSizes
    fromList [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]
-}
