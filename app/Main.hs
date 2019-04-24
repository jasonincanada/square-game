{-# Language RecordWildCards #-}
{-# Language TemplateHaskell #-}

{- Partridge Squares -}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad.State
import           Control.Monad (when)
import           Data.Bifunctor (bimap)
import           Data.Char      (digitToInt)
import           Data.Function ((&))
import           Data.Semigroup ((<>))
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           System.Random
import           Helpers (filteredKeys, minBy, randomIndices)
import           SquareGame

file :: FilePath
file = "generation/squares/N8-888666688445522333178876768555777744.sqr"

data World = World { _board     :: Board
                   , _message   :: String
                   , _cellHover :: Maybe Cell

                   -- The set of shrouded Cells to highlight as a preview of what would be
                   -- revealed when the user clicks
                   , _cellsToClick :: CellSet

                   -- Cache our last render of the board so we don't recompute redundantly every tick
                   , _rendered    :: Picture
                   , _renderCount :: Int

                   -- The size of the square we're placing, if any
                   , _placing :: Maybe Size
                   }

makeLenses ''World

-- UI globals
windowHeight = 1000
windowWidth  = 1000
shiftX       = (-1) * boardscale * 72 / 2
shiftY       =        boardscale * 72 / 2

boardscale :: Float
boardscale = 10


main :: IO ()
main = do
  board <- fromFile file

  let started = randomDeshroud 1 board
  let clicked = click (0,0,8) SRight
  let world   = World
                  started
                  "default message"
                  Nothing
                  S.empty
                  Blank
                  0
                  Nothing

  let withCache = world & rendered .~ render world

  play window white 10 withCache displayBoard events step



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


window :: Display
window = InWindow "Partridge Square" size position
  where
    size         = (windowWidth, windowHeight)
    position     = (100, 100)


events :: Event -> World -> World
events event world = case processEvent event world of
                       Just world' -> world' & rendered .~ render world'
                                             & renderCount %~ (+1)

                       Nothing     -> world


-- Return (Just world) if it requires a re-render or Nothing if not
processEvent :: Event -> World -> Maybe World
processEvent event world = case event of
  EventMotion (x, y) -> Just $ world & message      .~ show (world ^. placing)
                                     & cellHover    .~ windowToCell x y
                                     & cellsToClick .~ clickables world x y

  -- TODO: Need to debounce the mousewheel events
  EventKey (MouseButton WheelUp) _ _ (x, y)
                     -> case world ^. placing of
                          Nothing -> Just $ world & placing .~ Just 2
                          Just 8  -> Nothing
                          Just x  -> Just $ world & placing .~ Just (x+1)

  EventKey (MouseButton WheelDown) _ _ (x, y)
                     -> case world ^. placing of
                          Nothing -> Just $ world & placing .~ Just 2
                          Just 1  -> Nothing
                          Just x  -> Just $ world & placing .~ Just (x-1)

  EventKey (MouseButton LeftButton) Down _ (x, y)
                     -> Just $ leftClick world

  EventKey (Char '0') Down _ _
                     -> Just $ world & placing .~ Nothing

  EventKey (Char  c ) Down _ _
                     -> if c `elem` "12345678"
                        then Just $ world & placing .~ Just (digitToInt c)
                        else Nothing

  _                  -> Nothing


-- User clicked, so show the cells we've already determined can be deshrouded (during mouseover),
-- then do to the three types of auto-revealing until there are no further changes to do
leftClick :: World -> World
leftClick = execState click
  where
    click :: State World ()
    click = do
      clicked <- gets $ view cellsToClick

      modify $ over board $ deshroudCells clicked
                            >>> execState (sweepBoard clicked)

      modify $ set cellsToClick S.empty


    sweepBoard :: CellSet -> State Board ()
    sweepBoard cells = do
      grid <- gets $ view grid

      -- Since we're using Data.Set's map, this will de-dupe the resulting list for us,
      -- which may leave the set smaller than the list--this is fine
      let affectedSquares = S.map (\cell -> fst $ grid M.! cell) cells

      mapM_ sweepSquare affectedSquares


    sweepSquare :: Square -> State Board ()
    sweepSquare square = do

      before <- shroudsize square
      sweepEdges square
      sweepEight square
      sweepBorder square
      after <- shroudsize square

      -- Keep recursing until a round of sweeping has no effect
      when (before /= after) (sweepSquare square)


    shroudsize :: Square -> State Board Int
    shroudsize square = do
      squares <- gets $ view squares
      let shroud = fst $ squares M.! square
      return $ S.size shroud


    sweepEdges :: Square -> State Board ()
    sweepEdges square = sweep square STop
                     >> sweep square SRight
                     >> sweep square SBottom
                     >> sweep square SLeft


    sweep :: Square -> SquareSide -> State Board ()
    sweep square edge = do
      squares <- gets $ view squares

      let cellsets = squares M.! square
      let swept    = sweepEdge square edge cellsets

      modify $ deshroudCells swept


    sweepEight :: Square -> State Board ()
    sweepEight square = do
      squares <- gets $ view squares

      let cellsets   = squares M.! square
      let unveilable = any (sweepableEight square cellsets) [STop, SRight, SBottom, SLeft]

      when unveilable (modify $ deshroudCells (fst cellsets))


    sweepBorder :: Square -> State Board ()
    sweepBorder square = do
      squares <- gets $ view squares

      let shroud = fst $ squares M.! square
      let swept  = borderShroud shroud

      modify $ deshroudCells swept


step :: Float -> World -> World
step float = id

boardToWindow :: CRow -> CCol -> (Float, Float)
boardToWindow row col = (x, y)
  where
    x = fromIntegral col    * boardscale + shiftX
    y = fromIntegral (-row) * boardscale + shiftY

translateToSquareCenter :: SRow -> SCol -> Size -> Picture -> Picture
translateToSquareCenter row col size = Translate x y
  where
    x = boardscale * (2*  c  + s) + shiftX - 10
    y = boardscale * (2*(-r) - s) + shiftY - 10
    c = fromIntegral col
    r = fromIntegral row
    s = fromIntegral size

toWindowY :: Square -> SquareSide -> Float
toWindowY (row, _, _   ) STop    = fromIntegral (-row       ) * 2 * boardscale + shiftY
toWindowY (row, _, size) SBottom = fromIntegral (-(row+size)) * 2 * boardscale + shiftY

toWindowX :: Square -> SquareSide -> Float
toWindowX (_, col, _   ) SLeft   = fromIntegral  col          * 2 * boardscale + shiftX
toWindowX (_, col, size) SRight  = fromIntegral (col+size   ) * 2 * boardscale + shiftX

windowToCell :: Float -> Float -> Maybe Cell
windowToCell x y
  |    row >= 0 && row < 72
    && col >= 0 && col < 72 = Just (row, col)
  | otherwise               = Nothing
  where
    row = floor $ (y - shiftY) / boardscale * (-1)
    col = floor $ (x - shiftX) / boardscale

windowToSquareEdge :: World -> Float -> Float -> Maybe (Square, SquareSide)
windowToSquareEdge world x y = do
  cell <- windowToCell x y
  let square = fst $ (world ^. board ^. grid) M.! cell

  let distancesToEdges = let top    = toWindowY square STop
                             bottom = toWindowY square SBottom
                             left   = toWindowX square SLeft
                             right  = toWindowX square SRight
                         in  [ (abs $ y-top,    STop   )
                             , (abs $ y-bottom, SBottom)
                             , (abs $ x-left,   SLeft  )
                             , (abs $ x-right,  SRight ) ]

  return (square, minBy fst snd distancesToEdges)


clickables :: World -> Float -> Float -> CellSet
clickables world x y = cells
  where
    squares' = world ^. board ^. squares
    cells = case windowToSquareEdge world x y of
              Nothing             -> S.empty
              Just (square, edge) -> if fullyRevealed square
                                     then getFor square edge
                                     else S.empty

    fullyRevealed :: Square -> Bool
    fullyRevealed square = S.empty == fst (squares' M.! square)

    getFor :: Square -> SquareSide -> CellSet
    getFor square edge = intersect
      where
        intersect = S.intersection all shrouded
        all       = click square edge
        shrouded  = foldr S.union S.empty (M.elems $ M.map fst squares')


displayBoard :: World -> Picture
displayBoard world = world ^. rendered

render :: World -> Picture
render world = picture
  where
    Board squares grid = world ^. board

    picture      = if placeableSquare == Nothing
                   then mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ cellHoveredOver
                               ++ deshroudableCells
                               ++ msg
                   else mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ cellHoveredOver
                               ++ placingSquare
                               ++ msg

    full         = fullSquares squares


    placingSquare :: [Picture]
    placingSquare = case placeableSquare of
                      Nothing     -> mempty
                      Just square -> [Color yellow $ renderFull square]

    placeableSquare :: Maybe Square
    placeableSquare = do
      size         <- world ^. placing
      (crow, ccol) <- world ^. cellHover

      return $ let row = crow `div` 2 - size `div` 2
                   col = ccol `div` 2 - size `div` 2
               in  clampSquare (row, col, size)


    deshroudableCells :: [Picture]
    deshroudableCells = map (Color green . renderShroud) (S.toList $ world ^. cellsToClick)

    cellHoveredOver :: [Picture]
    cellHoveredOver = case world ^. cellHover of
      Nothing     -> [Blank]
      Just (r, c) -> [ Color red $ Polygon [ boardToWindow r     c
                                           , boardToWindow (r+1) c
                                           , boardToWindow (r+1) (c+1)
                                           , boardToWindow r     (c+1)
                                           , boardToWindow r     c
                                           ] ]

    -- Render a fully-unshrouded square
    renderFull :: Square -> Picture
    renderFull (row, col, size) = line <> digit
      where
        line  = Line [ boardToWindow r     c
                     , boardToWindow (r+s) c
                     , boardToWindow (r+s) (c+s)
                     , boardToWindow r     (c+s)
                     , boardToWindow r     c ]

        digit = translateToSquareCenter row col size
                  $ Scale 0.2 0.2
                  $ Text (show size)

        r = 2*row
        c = 2*col
        s = 2*size

    renderShroud :: Cell -> Picture
    renderShroud (row, col) =
      Polygon [ boardToWindow row     col
              , boardToWindow row     (col+1)
              , boardToWindow (row+1) (col+1)
              , boardToWindow (row+1) col
              , boardToWindow row     col
              ]

    renderUnshroud :: (Cell, CellBorder) -> Picture
    renderUnshroud ((row, col), border) = case border of
      CTopLeft     -> Line [ boardToWindow  row    (col+1)
                           , boardToWindow  row     col
                           , boardToWindow (row+1)  col    ]
      CTop         -> Line [ boardToWindow  row     col
                           , boardToWindow  row    (col+1) ]
      CTopRight    -> Line [ boardToWindow  row     col
                           , boardToWindow  row    (col+1)
                           , boardToWindow (row+1) (col+1) ]
      CRight       -> Line [ boardToWindow  row    (col+1)
                           , boardToWindow (row+1) (col+1) ]
      CBottomRight -> Line [ boardToWindow  row    (col+1)
                           , boardToWindow (row+1) (col+1)
                           , boardToWindow (row+1)  col    ]
      CBottom      -> Line [ boardToWindow (row+1)  col
                           , boardToWindow (row+1) (col+1) ]
      CBottomLeft  -> Line [ boardToWindow  row     col
                           , boardToWindow (row+1)  col
                           , boardToWindow (row+1) (col+1) ]
      CLeft        -> Line [ boardToWindow  row     col
                           , boardToWindow (row+1)  col    ]
      CNone        -> Blank


    msg :: [Picture]
    msg = [ Translate (-350) 380
              $ Scale 0.2 0.2
              $ Text
              $ show (world ^. renderCount) ++ ", " ++ (world ^. message) ]

    shroud       = concatMap (S.toList . fst) (M.elems squares)
    unshroud     = concatMap (S.toList . snd) (M.elems squares)

    unshrouded   = map (\cell -> (cell, snd $ grid M.! cell)) unshroud

