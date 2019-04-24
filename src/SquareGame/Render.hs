module SquareGame.Render (
    render
  , windowHeight
  , windowWidth
  , windowToCell
  , windowToSquareEdge
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Lens
import           Graphics.Gloss
import           Helpers (minBy)
import           SquareGame
import           SquareGame.World

-- UI globals
windowHeight = 1000 :: Int
windowWidth  = 1000 :: Int
shiftX       = (-1) * boardscale * 72 / 2
shiftY       =        boardscale * 72 / 2

boardscale :: Float
boardscale = 10


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
