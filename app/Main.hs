{-# Language RecordWildCards #-}

{- Partridge Squares -}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Bifunctor (bimap)
import           Data.List      (minimumBy)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           SquareGame

file :: FilePath
file = "generation/squares/N8-888666688445522333178876768555777744.sqr"

data World = World { board     :: Board
                   , message   :: String
                   , cellHover :: Maybe Cell

                   -- The set of shrouded Cells to highlight as a preview of what would be
                   -- revealed when the user clicks
                   , cellsToClick :: [Cell]
                   }

-- UI globals
windowHeight = 1000
windowWidth  = 1000
shiftX       = (-1) * boardscale * 72 / 2
shiftY       =        boardscale * 72 / 2

boardscale :: Float
boardscale = 10


main :: IO ()
main = do
  board <- deshroud (0,0,8) . deshroud (0,30,6) <$> fromFile file
  let clicked = click (0,0,8) SRight
  let board' = deshroudCells board (clicked ++ map fst (cells (0, 0, 36)))
  let world = World board' "default message" Nothing []

  play window white 20 world displayBoard events step

window :: Display
window = InWindow "Partridge Square" size position
  where
    size         = (windowWidth, windowHeight)
    position     = (100, 100)

events :: Event -> World -> World
events event world = case event of
  EventMotion (x, y) -> world { message      = show $ clickables world x y
                              , cellHover    = windowToCell x y
                              , cellsToClick = clickables world x y
                              }
  _                  -> world

step :: Float -> World -> World
step float = id

boardToWindow :: Row -> Col -> (Float, Float)
boardToWindow row col = (x, y)
  where
    x = (fromIntegral col)    * boardscale + shiftX
    y = (fromIntegral (-row)) * boardscale + shiftY

translateToSquareCenter :: Row -> Col -> Size -> Picture -> Picture
translateToSquareCenter row col size pic = Translate x y pic
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
toWindowX (_, col, _   ) SLeft   = fromIntegral (col        ) * 2 * boardscale + shiftX
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
windowToSquareEdge (World (Board _ grid) _ _ _) x y = do
  cell <- windowToCell x y
  let square = fst $ grid M.! cell

  let distancesToEdges = let top    = toWindowY square STop
                             bottom = toWindowY square SBottom
                             left   = toWindowX square SLeft
                             right  = toWindowX square SRight
                         in  [ (abs $ y-top,    STop   )
                             , (abs $ y-bottom, SBottom)
                             , (abs $ x-left,   SLeft  )
                             , (abs $ x-right,  SRight ) ]

  return (square, minBy fst snd distancesToEdges)

minBy :: Ord b => (a -> b) -> (a -> c) -> [a] -> c
minBy measure finalize list = go list
  where
    go [a]                     = finalize a
    go (a:a':rest)
      | measure a < measure a' = go (a :rest)
      | otherwise              = go (a':rest)


clickables :: World -> Float -> Float -> [Cell]
clickables world@(World (Board squares _) _ _ _) x y = cells
  where
    cells = case windowToSquareEdge world x y of
              Nothing             -> []
              Just (square, edge) -> if fullyRevealed square
                                     then getFor square edge
                                     else []

    fullyRevealed :: Square -> Bool
    fullyRevealed square = S.empty == fst (squares M.! square)

    getFor :: Square -> SquareSide -> [Cell]
    getFor square edge = S.toList intersect
      where
        intersect = S.intersection all shrouded
        all       = S.fromList $ click square edge
        shrouded  = foldr S.union S.empty (M.elems $ M.map fst squares)

displayBoard :: World -> Picture
displayBoard World{..} = picture
  where
    Board squares grid = board

    picture      = mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ cellHoveredOver
                               ++ deshroudableCells
                               ++ msg

    full         = fullSquares squares

    deshroudableCells :: [Picture]
    deshroudableCells = map (Color green . renderShroud) cellsToClick

    cellHoveredOver :: [Picture]
    cellHoveredOver = case cellHover of
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
    msg = [ Translate (-350) (380)
              $ Scale 0.2 0.2
              $ Text message
          ]

    shroud       = concat . map S.toList $ map fst (M.elems squares)
    unshroud     = concat . map S.toList $ map snd (M.elems squares)

    unshrouded   = map (\cell -> (cell, snd $ grid M.! cell)) unshroud

