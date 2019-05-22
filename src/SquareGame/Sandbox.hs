{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

{- Sandbox.hs  - Sandbox enviro for naming/addressing all N=8 partridge squares -}

module SquareGame.Sandbox where

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Set    as S
import           Data.Aeson
import           Data.Function  ((&))
import           Data.List      (elemIndex, sortBy)
import           Data.Ord       (comparing)
import           Data.Maybe     (fromJust)
import           Text.Printf    (printf)
import           GHC.Generics
import           SquareGame

type Height      = Int
type Width       = Int
type Rectangle   = (SRow, SCol, Height, Width)
type SizeMap     = IM.IntMap Int
type RegionName  = String
type FamilyName  = String

data Region = Region { squares    :: [(Size, Int)]
                     , rectangles :: [Rectangle]
                     , tilings    :: [Tiling]
                     } deriving (Generic, Show)

data RegionMap = RegionMap { regionMap :: M.Map RegionName Region }
                 deriving (Generic, Show)


bow        = Region [(4,2),(6,2),(7,4),(8,4)]       [(0,16,6,12), (6,0,15,28), (13,28,8,8) ] []
garden     = Region [(2,2),(3,2),(4,2),(5,2),(6,1)] [(0,0,16,9)]                             []
praline1   = Region [(2,2),(4,1),(6,1)]             [(0,0,6,10)]                             []
praline2   = Region [(2,2),(4,1),(6,2)]             [(0,0,6,16)]                             []
whitehouse = Region [(4,2),(5,4),(8,2)]             [(0,0,13,20)]                            []
flower     = Region [(3,2),(6,1),(7,4),(8,4)]       [(0,0,23,22)]                            []


-- A tiling is a left-right/top-down placement of squares across a Region
type Tiling = S.Set Square

data Family = Family {
                     -- symmetricRegions defines the family
                     symmetricRegions :: [(String, Tile)]

                     -- These are determined by symmetricRegions, we'll compute them
                     -- and cache the results to file
                     , frameTilings     :: [Tiling]

                     } deriving (Generic, Show)

data FamilyMap = FamilyMap { familyMap :: M.Map FamilyName Family }
                 deriving (Generic, Show)

data BoardNames = BoardNames { nameMap :: M.Map String [String] }
                  deriving (Generic, Show)


instance ToJSON   BoardNames where toEncoding = genericToEncoding defaultOptions
instance ToJSON   Family     where toEncoding = genericToEncoding defaultOptions
instance ToJSON   FamilyMap  where toEncoding = genericToEncoding defaultOptions
instance ToJSON   Region     where toEncoding = genericToEncoding defaultOptions
instance ToJSON   RegionMap  where toEncoding = genericToEncoding defaultOptions
instance FromJSON BoardNames
instance FromJSON Family
instance FromJSON FamilyMap
instance FromJSON Region
instance FromJSON RegionMap

family1 :: Family
family1 = Family [ ("bow",    (15, 0))
                 , ("garden", (0, 0))
                 ]
                 []

family2 :: Family
family2 = Family [ ("2-praline", (0, 0) )
                 , ("whitehouse", (0, 16))
                 , ("flower", (13, 14))
                 ]
                 []

family3 :: Family
family3 = Family [ ("1-praline", (0, 0) )
                 , ("flower", (13, 0))
                 ]
                 []


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


tilesFor :: [Rectangle] -> TileSet
tilesFor rectangles = foldr S.union S.empty $ for <$> rectangles
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


--- The tiles left over after the square's symmetrical regions are carved out
frame :: Family -> M.Map RegionName Region -> TileSet
frame (Family regions _) regionMap = wholeBoard `S.difference` used
  where
    wholeBoard :: TileSet
    wholeBoard = tilesFor [(0,0,36,36)]

    used :: TileSet
    used = foldr (S.union . tiles) S.empty regions
      where
        tiles :: (String, Tile) -> TileSet
        tiles (name, (row, col)) = let areas      = rectangles $ regionMap M.! name
                                       translated = map (offset row col) areas
                                   in  tilesFor translated


-- Translate a rectangle away from the top-left corner
offset :: SRow -> SCol -> Rectangle -> Rectangle
offset dr dc (row, col, height, width) = (row+dr, col+dc, height, width)


--- The squares remaining after the symmetrical regions' squares are used
frameSquares :: Family -> M.Map RegionName Region -> SizeMap
frameSquares (Family regions _) regionMap = without $ map toSizeMap regions
  where
    toSizeMap :: (RegionName, Tile) -> SizeMap
    toSizeMap (name, _) = IM.fromList
                            $ SquareGame.Sandbox.squares
                            $ regionMap M.! name


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

algebra :: Algebra TrieF [Tiling]
algebra (NodeF []    ) = [S.empty]
algebra (NodeF boards) = concatMap addsquare boards
  where
    addsquare :: (Square, [Tiling]) -> [Tiling]
    addsquare (square, sets) = map (S.insert square) sets

hylo :: Functor f => Coalgebra f a -> Algebra f b -> a -> b
hylo coalg alg = alg . (fmap $ hylo coalg alg) . coalg


-- Find all placements that cover the whole region being tiled
-- (not necessarily using all available squares)
tiled :: TileSet -> SizeMap -> [Tiling]
tiled tiles sizes = filter fullyTiled results
  where
    results :: [Tiling]
    results = hylo coalgebra algebra (tiles, sizes)

    fullyTiled :: Tiling -> Bool
    fullyTiled squares = totalTiles == S.size tiles
      where
        totalTiles             = sum $ map tileCount (S.elems squares)
        tileCount (_, _, size) = size * size


-- Transpose (flip along the row==col diagonal)
tau :: Tiling -> Tiling
tau = S.map t
  where
    t :: Square -> Square
    t (row, col, size) = (col, row, size)

-- Rotate clockwise a quarter turn
rho :: Tiling -> Tiling
rho = S.map (\(row, col, size) -> (col, side - row - size, size))
  where
    side = 36 -- For n=8

-- Call these successively to return the 8 symmetries of the board
d4 :: [(String, Tiling -> Tiling)]
d4 = [ ("e"  , id      )
     , ("r"  , rho     )
     , ("r2" , rho     )
     , ("r3" , rho     )
     , ("t"  , tau.rho )
     , ("rt" , rho     )
     , ("r2t", rho     )
     , ("r3t", rho     ) ]

getSymmetries :: (String, Tiling) -> [(String, Tiling)]
getSymmetries (prefix, tiling) = go d4 tiling
  where
    go :: [(String, Tiling -> Tiling)] -> Tiling -> [(String, Tiling)]
    go [] _                    = []
    go ((name, f):rest) tiling = let new = f tiling
                                 in  (prefix ++ " " ++ name, new) : go rest new



-- Get the ith tiling of a region from disk
region :: RegionName -> Int -> IO Tiling
region name i = do
  regions <- getRegions

  return $ (tilings $ regions M.! name) !! (i-1)


----------
--- IO ---
----------

fileRegions, fileFamilies :: FilePath
fileRegions  = "regions.json"
fileFamilies = "families.json"
fileNames    = "names.json"

-- Getters
getRegion :: RegionName -> IO Region
getRegion name = fromJust . M.lookup name <$> getRegions

getRegions :: IO (M.Map RegionName Region)
getRegions = regionMap . fromJust <$> decodeFileStrict fileRegions

getFamilies :: IO (M.Map FamilyName Family)
getFamilies = familyMap . fromJust <$> decodeFileStrict fileFamilies

getBoardNames :: IO (M.Map String [String])
getBoardNames = nameMap . fromJust <$> decodeFileStrict fileNames


-- Updaters
updateFamily :: FamilyName -> Family -> IO ()
updateFamily name family = do
  families <- getFamilies

  let updated = M.insert name family families

  encodeFile fileFamilies (FamilyMap updated)

updateRegion :: RegionName -> Region -> IO ()
updateRegion name region = do
  regions  <- getRegions

  let updated = M.insert name region regions

  encodeFile fileRegions (RegionMap updated)


-- Tile the frame for a family, saving to disk what it finds
tileFrame :: FamilyName -> IO ()
tileFrame name = do
  regions  <- getRegions
  families <- getFamilies

  let family  = families M.! name
  let tiles   = frame        family regions
  let squares = frameSquares family regions
  let found   = tiled tiles squares
  let updated = family { frameTilings = found }

  updateFamily name updated

  putStrLn $ "Found " ++ show (length found)



tileRegion :: RegionName -> IO ()
tileRegion name = do
  regions  <- getRegions

  let region  = regions M.! name
  let tiles   = tilesFor (rectangles region)
  let found   = tiled tiles (IM.fromList $ SquareGame.Sandbox.squares region)
  let updated = region { tilings = found   }

  updateRegion name updated

  putStrLn $ "Found " ++ show (length found)


-- Write some newly found board names to disk
setBoardNames :: [(String, String)] -> IO ()
setBoardNames names = do
  boardNames <- getBoardNames

  let updated = foldr add boardNames names

  encodeFile fileNames (BoardNames updated)

  where
    add :: (String, String) -> M.Map String [String] -> M.Map String [String]
    add (name, path) map
      | name `elem` (map M.! path) = map
      | otherwise                  = M.adjust (name:) path map


allBoards :: FamilyName -> IO [(String, String)]
allBoards name = do
  regions  <- getRegions
  families <- getFamilies

  let family = families M.! name
  let symms  = symmetricRegions family & sortBy (comparing snd)

  let frameFactors  = framesToFactors name (frameTilings family)  -- :: [Factor]
  let regionFactors = map (regionToFactors regions) symms         -- :: [[Factor]]
  let factors       = frameFactors : regionFactors

  -- sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
  let sequenced     = sequence factors                            -- :: [[Factor]]
  let boards        = map toBoard sequenced                       -- :: [(String, Tiling)]

  -- This isn't the most efficient way to do this since it rotates the frame
  -- every time redundantly, but for our dataset size it's not a bottleneck
  let allSymmetries = concatMap getSymmetries boards

  pure $ [ (name, path tiling) | (name, tiling) <- allSymmetries ]


type Factor = (String, Int, Tiling)


framesToFactors :: String -> [Tiling] -> [Factor]
framesToFactors name tilings = [ (name, i, tiling) | (i, tiling) <- zip [1..] tilings ]

regionToFactors :: M.Map RegionName Region -> (String, Tile) -> [Factor]
regionToFactors regionMap (name, (row, col)) = factors
  where
    factors = [ (name, i, translate row col tiling) | (i, tiling) <- zip [1..] ts ]
    region  = regionMap M.! name
    ts      = tilings region

    translate :: SRow -> SCol -> Tiling -> Tiling
    translate row col = S.map adjust
      where
        adjust (r, c, size) = (row+r, col+c, size)


toBoard :: [Factor] -> (String, Tiling)
toBoard factors = foldr f ("", S.empty) factors
  where
    f :: Factor -> (String, Tiling) -> (String, Tiling)
    f (name, index, tiling) (name', tiling')
      | null name'   = (name ++ "-" ++ show index                ,         tiling        )
      | otherwise    = (name ++ "-" ++ show index ++ " " ++ name', S.union tiling tiling')


path :: Tiling -> String
path = concatMap (show . size) . S.toList



-- Info

--
showFamily :: FamilyName -> IO ()
showFamily name = do
  families <- getFamilies
  regions  <- getRegions

  let g (name, _)  = length $ tilings $ regions M.! name

  let family       = families M.! name
  let symmetrics   = symmetricRegions family
  let frameTCount  = length  $ frameTilings family
  let regionTCount = product $ map g symmetrics

  let f (name, (row, col)) = printf " %3d tilings of region: %s @%d,%d"
                                    (length $ tilings $ regions M.! name)
                                    name
                                    row
                                    col

  putStrLn $ printf "%s:" name
  putStrLn $ printf " %3d tilings of the frame" frameTCount
  putStr   $ unlines $ map f symmetrics
  putStrLn $ "   8 symmetries of the board"
  putStrLn $ "------------------------------"
  putStrLn $ printf "%4d total squares" (8 * frameTCount * regionTCount)


-- Get the next unnamed board
nextBoard :: IO (Int, String)
nextBoard = do
  boardNames <- getBoardNames

  let unnamed = M.filter null boardNames
  let first   = head $ M.keys unnamed
  let idx     = fromJust $ elemIndex first $ M.keys boardNames

  pure (idx + 1, first)



{-
    λ> nextBoard
    (47,"226885544673155544677886833886687777")

    λ> family3
    Family {symmetricRegions = [("1-praline",(0,0)),("flower",(13,0))], frameTilings = []}

    λ> showFamily "family-3"
    family-3:
       1 tilings of the frame
       4 tilings of region: 1-praline @0,0
       4 tilings of region: flower @13,0
       8 symmetries of the board
    ------------------------------
     128 total squares


    ------
    λ> names <- allBoards "family-2"
    λ> length names
    1152
    λ> head names
    ("family-2-1 2-praline-1 whitehouse-1 flower-1 e","226648844547555513687786833886687777")
    λ> last names
    ("family-2-1 2-praline-6 whitehouse-6 flower-4 r3t","777786883386688776375555514884662244")


    ------
    λ> updateRegion "2-praline" praline2
    λ> updateRegion "whitehouse" whitehouse
    λ> updateRegion "flower" flower

    λ> tileRegion "2-praline"
    Found 6

    λ> tileRegion "whitehouse"
    Found 6

    λ> tileRegion "flower"
    Found 4

    λ> tileFrame "family-2"
    Found 1

    λ> showFamily "family-2"
    family-2:
       1 tilings of the frame
       6 tilings of region: 2-praline @0,0
       6 tilings of region: whitehouse @0,16
       4 tilings of region: flower @13,14
       8 symmetries of the board
    ------------------------------
    1152 total squares


    ------
    λ> nextBoard
    (23,"226648844547555513687786833886687777")


    ------
    λ> names <- allBoards "family-1"
    λ> :t names
    names :: [(String, String)]
    λ> length names
    2112
    λ> setBoardNames names
    λ> boardmap <- nameMap . fromJust <$> getBoardNames
    λ> boardmap M.! "225777645468853638316485548886687777"
    ["family-1-1 garden-1 bow-1 e"]


    ------
    λ> garden <- region "garden" 1
    λ> garden
    fromList [(0,0,2),(0,2,2),(0,4,5),(2,0,4),(5,4,5),(6,0,4),(10,0,3),(10,3,6),(13,0,3)]

    λ> tau garden
    fromList [(0,0,2),(0,2,4),(0,6,4),(0,10,3),(0,13,3),(2,0,2),(3,10,6),(4,0,5),(4,5,5)]
    λ> rho garden
    fromList [(0,20,3),(0,23,3),(0,26,4),(0,30,4),(0,34,2),(2,34,2),(3,20,6),(4,26,5),(4,31,5)]

    λ> (tau . tau) garden == garden
    True

    λ> garden == iterate rho garden !! 3
    False
    λ> garden == iterate rho garden !! 4
    True


    ------
    λ> (!! 0) <$> allBoards "family-1"
    λ> (!! 1) <$> allBoards "family-1"
    λ> (!! 7) <$> allBoards "family-1"
    λ> (!! 8) <$> allBoards "family-1"
    ("family-1-1 garden-1 bow-1 e","225777645468853638316485548886687777")
    ("family-1-1 garden-1 bow-1 r","785334422655578187676447887673588866")
    ("family-1-1 garden-1 bow-1 r3t","777788866848556843618833564577746522")
    ("family-1-1 garden-1 bow-2 e","225777645468853638316665584888487777")

    λ> last <$> allBoards "family-1"
    ("family-1-1 garden-24 bow-11 r3t","777788866884556845221883456547776363")

    λ> length <$> allBoards "family-1"
    2112


    ------
    λ> showFamily "family-1"
    family-1:
       1 tilings of the frame
      11 tilings of region: bow @15,0
      24 tilings of region: garden @0,0
       8 symmetries of the board
    ------------------------------
    2112 total squares


    ------
    λ> getFamilies
    Just (FamilyMap {familyMap = fromList [("family-1",Family {symmetricRegions = [("bow",(15,0)),("garden",(0,0))], frameTilings = []})]})

    λ> tileFrame "family-1"
    [fromList [(0,9,7),(0,16,7),(0,23,7),(0,30,6),(6,30,6),(7,9,8),(7,17,8),(7,25,5),(12,25,3),(12,28,8),(15,9,1),(15,10,6),(16,0,5),(16,5,5),(20,28,8)]]

    λ> getFamilies
    Just (FamilyMap {familyMap = fromList [("family-1",Family {symmetricRegions = [("bow",(15,0)),("garden",(0,0))], frameTilings = [fromList [(0,9,7),(0,16,7),(0,23,7),(0,30,6),(6,30,6),(7,9,8),(7,17,8),(7,25,5),(12,25,3),(12,28,8),(15,9,1),(15,10,6),(16,0,5),(16,5,5),(20,28,8)]]})]})


    ------
    λ> tileRegion "bow"
    [fromList [(0,16,4),(0,20,8),(4,16,4),...,(14,7,7),(14,14,7),(14,21,7)], ...]

    λ> length <$> tileRegion "bow"
    11


    ------
    λ> tileFrame "family-1"
    [fromList [(0,9,7),(0,16,7),(0,23,7),(0,30,6),(6,30,6),(7,9,8),(7,17,8),(7,25,5),(12,25,3),(12,28,8),(15,9,1),(15,10,6),(16,0,5),(16,5,5),(20,28,8)]]


    ------
    λ> import Data.Maybe
    λ> regions  <- regionMap . fromJust <$> getRegions
    λ> families <- familyMap . fromJust <$> getFamilies

    λ> frame (families M.! "family-1") regions
    fromList [(0,9),(0,10),(0,11), ..., (27,33),(27,34),(27,35)]

    λ> length $ frame (families M.! "family-1") regions
    596


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
