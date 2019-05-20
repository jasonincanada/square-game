{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

{- Sandbox.hs  - Sandbox enviro for naming/addressing all N=8 partridge squares -}

module SquareGame.Sandbox where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S
import           Data.Aeson
import           GHC.Generics
import           SquareGame

type Height      = Int
type Width       = Int
type Rectangle   = (SRow, SCol, Height, Width)
type SizeMap     = IM.IntMap Int
type RegionName  = String
type FamilyName  = String

data Region = Region { squares    :: [(Size, Int)]
                     , rectangles :: [(Int, Int, Int, Int)]
                     } deriving (Generic, Show)

data RegionMap = RegionMap (M.Map RegionName Region)
                 deriving (Generic, Show)


regionMap :: RegionMap
regionMap = RegionMap map
  where
    map = M.fromList [ ("bow",    Region [(4,2),(6,2),(7,4),(8,4)]       [(0,16,6,12), (6,0,15,28), (13,28,8,8) ])
                     , ("garden", Region [(2,2),(3,2),(4,2),(5,2),(6,1)] [(0,0,16,9)])
                     ]


-- A tiling is a left-right/top-down placement of squares across a Region
type Tiling = S.Set Square

data Family = Family {
                     -- symmetricRegions defines the family
                     symmetricRegions :: [(String, Tile)]

                     -- These are determined by symmetricRegions, we'll compute them
                     -- and cache the results to file
                     , frameTilings     :: [Tiling]

                     } deriving (Generic, Show)

data FamilyMap = FamilyMap (M.Map FamilyName Family)
                 deriving (Generic, Show)

instance ToJSON   Family    where toEncoding = genericToEncoding defaultOptions
instance ToJSON   FamilyMap where toEncoding = genericToEncoding defaultOptions
instance ToJSON   Region    where toEncoding = genericToEncoding defaultOptions
instance ToJSON   RegionMap where toEncoding = genericToEncoding defaultOptions
instance FromJSON Family
instance FromJSON FamilyMap
instance FromJSON Region
instance FromJSON RegionMap

family1 :: Family
family1 = Family [ ("bow",    (15, 0))
                 , ("garden", (0, 0))
                 ]
                 []

familyMap :: FamilyMap
familyMap = FamilyMap map
  where
    map = M.fromList [("family-1", family1)]


--allSquares :: [Size]
--allSquares = concat [ replicate i i | i <- [1..8] ]
allSizes :: SizeMap
allSizes = IM.fromList [ (i, i) | i <- [1..8] ]

bowSizes :: SizeMap
bowSizes = IM.fromList [ (4,2), (6,2), (7,4), (8,4) ]

gardenSizes :: SizeMap
gardenSizes = IM.fromList [ (2,2), (3,2), (4,2), (5,2), (6,1) ]

-- Remove some squares from the original pile
without :: [SizeMap] -> SizeMap
without = removeZeroes . foldl (IM.unionWith (-)) allSizes
  where
    removeZeroes :: SizeMap -> SizeMap
    removeZeroes = IM.filter (/= 0)


tilesFor :: Region -> TileSet
tilesFor (Region _ areas) = foldr S.union S.empty $ for <$> areas
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


--- The tiles left over after the two symmetrical regions in family 1 squares are carved out
{-
family1 :: TileSet
family1 = wholeBoard `S.difference` used
  where
    wholeBoard :: TileSet
    wholeBoard = tilesFor [(0,0,36,36)]

    used :: TileSet
    used = S.union garden' bow'
      where
        garden' = tilesFor $ offset 0  0 garden
        bow'    = tilesFor $ offset 15 0 bow


-- Translate a region away from the top-left corner
offset :: SRow -> SCol -> Region -> Region
offset dr dc = map f
  where
    f (row, col, height, width) = (row+dr, col+dc, height, width)

-}


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
                            row+size <= 36 && col+size <= 36,

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

        decide :: Size -> Int -> (SizeMap -> SizeMap)
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


-- Find all placements that cover the whole region being tiled
-- (not necessarily using all available squares)
tiled :: TileSet -> SizeMap -> [S.Set Square]
tiled tiles sizes = filter fullyTiled results
  where
    results :: [S.Set Square]
    results = hylo coalgebra algebra (tiles, sizes)

    fullyTiled :: S.Set Square -> Bool
    fullyTiled squares = totalTiles == S.size tiles
      where
        totalTiles             = sum $ map tileCount (S.elems squares)
        tileCount (_, _, size) = size * size


----------
--- IO ---
----------

regions :: IO (Maybe RegionMap)
regions = decodeFileStrict "regions.json"

families :: IO (Maybe FamilyMap)
families = decodeFileStrict "families.json"


{-
    λ> familyMap
    FamilyMap (fromList [("family-1",Family {symmetricRegions = [("bow",(15,0)),("garden",(0,0))], frame = [], frameSquares = [], frameTilings = []})])

    λ> encodeFile "families.json" familyMap

    λ> families
    Just (FamilyMap (fromList [("family-1",Family {symmetricRegions = [("bow",(15,0)),("garden",(0,0))], frame = [], frameSquares = [], frameTilings = []})]))


    ------
    λ> import Data.Aeson
    λ> encodeFile "regions.json" regionMap
    λ> regions
    Just (RegionMap (fromList [("bow",Region {squares = [(4,2),(6,2),(7,4),(8,4)], rectangles = [(0,16,6,12),(6,0,15,28),(13,28,8,8)]}),("garden",Region {squares = [(2,2),(3,2),(4,2),(5,2),(6,1)], rectangles = [(0,0,16,9)]})]))


    ------
    λ> tiled family1 (without [bowSizes, gardenSizes])
    [fromList [(0,9,7),(0,16,7),(0,23,7),(0,30,6),(6,30,6),(7,9,8),(7,17,8),(7,25,5),(12,25,3),(12,28,8),(15,9,1),(15,10,6),(16,0,5),(16,5,5),(20,28,8)]]

    λ> length $ tiled family1 (without [bowSizes, gardenSizes])
    1


    ------
    λ> length $ tiled (tilesFor bow) bowSizes
    11

    λ> head $ tiled (tilesFor bow) bowSizes
    fromList [(0,16,4),(0,20,8),(4,16,4),(6,0,8),(6,8,8),(8,16,6),(8,22,6),(13,28,8),(14,0,7),(14,7,7),(14,14,7),(14,21,7)]


    ------
    λ> tiled (tilesFor top2x2)
    [fromList [(0,0,2)]]

    λ> length $ tiled (tilesFor garden)
    24

    λ> head $ tiled (tilesFor garden)
    fromList [(0,0,2),(0,2,2),(0,4,5),(2,0,4),(5,4,5),(6,0,4),(10,0,3),(10,3,6),(13,0,3)]


    ------
    λ> hylo coalgebra algebra (tilesFor top2x2, allSizes)
    [fromList [(0,0,1)],fromList [(0,0,2)]]

    λ> tilesFor top2x2
    fromList [(0,0),(0,1),(1,0),(1,1)]

    λ> allSizes
    fromList [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8)]
-}
