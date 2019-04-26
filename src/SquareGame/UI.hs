module SquareGame.UI (
    cellBorderPath
  , clickables
  , squareBorderPath
  , squareDigit
  , windowToCell
  , windowWidth
  , windowHeight
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Arrow  ((>>>))
import           Data.Bifunctor (bimap)
import           Helpers        (minBy)
import           SquareGame

{- UI globals -}
boardScale :: Float
boardScale = 10

windowHeight = 1000 :: Int
windowWidth  = 1000 :: Int
shiftX       = (-1) * boardScale * 72 / 2
shiftY       =        boardScale * 72 / 2


boardToWindow :: CRow -> CCol -> (Float, Float)
boardToWindow row col = (x, y)
  where
    x = fromIntegral col    * boardScale + shiftX
    y = fromIntegral (-row) * boardScale + shiftY


windowToCell :: Float -> Float -> Maybe Cell
windowToCell x y
  |    row >= 0 && row < 72
    && col >= 0 && col < 72 = Just (row, col)
  | otherwise               = Nothing
  where
    row = floor $ (y - shiftY) / boardScale * (-1)
    col = floor $ (x - shiftX) / boardScale


windowToSquareEdge :: CellGrid -> Float -> Float -> Maybe (Square, SquareSide)
windowToSquareEdge grid x y = do
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


toWindowY :: Square -> SquareSide -> Float
toWindowY (row, _, _   ) STop    = fromIntegral (-row       ) * 2 * boardScale + shiftY
toWindowY (row, _, size) SBottom = fromIntegral (-(row+size)) * 2 * boardScale + shiftY

toWindowX :: Square -> SquareSide -> Float
toWindowX (_, col, _   ) SLeft   = fromIntegral  col          * 2 * boardScale + shiftX
toWindowX (_, col, size) SRight  = fromIntegral (col+size   ) * 2 * boardScale + shiftX


-- Return the (x,y) where the digit for this square should be drawn
squareDigit :: Square -> (Float, Float)
squareDigit = squareCenter >>> bimap slide slide
  where
    slide      = subtract (digitWidth / 2)
    digitWidth = 16

squareCenter :: Square -> (Float, Float)
squareCenter (row, col, size) = (x, y)
  where
    x = boardScale * (2*  c  + s) + shiftX
    y = boardScale * (2*(-r) - s) + shiftY
    c = fromIntegral col
    r = fromIntegral row
    s = fromIntegral size


clickables :: Float -> Float -> Board -> Maybe CellSet
clickables x y (Board squares grid) = cells
  where
    cells = case windowToSquareEdge grid x y of
              Nothing             -> Nothing
              Just (square, edge) -> if fullyRevealed square
                                     then Just $ getFor square edge
                                     else Nothing

    fullyRevealed :: Square -> Bool
    fullyRevealed square = S.empty == fst (squares M.! square)

    getFor :: Square -> SquareSide -> CellSet
    getFor square edge = intersect
      where
        intersect = S.intersection all shrouded
        all       = click square edge
        shrouded  = foldr S.union S.empty (M.elems $ M.map fst squares)


type Path = [(Float, Float)]

squareBorderPath :: Square -> Path
squareBorderPath (r, c, s) =
  [ boardToWindow  row        col
  , boardToWindow (row+size)  col
  , boardToWindow (row+size) (col+size)
  , boardToWindow  row       (col+size)
  , boardToWindow  row        col ]
  where
    row  = 2*r
    col  = 2*c
    size = 2*s


cellBorderPath :: Cell -> CellBorder -> Path
cellBorderPath (row, col) border = case border of
  CTopLeft     -> [ boardToWindow (row+1)  col
                  , boardToWindow  row     col
                  , boardToWindow  row    (col+1) ]
  CTop         -> [ boardToWindow  row     col
                  , boardToWindow  row    (col+1) ]
  CTopRight    -> [ boardToWindow  row     col
                  , boardToWindow  row    (col+1)
                  , boardToWindow (row+1) (col+1) ]
  CRight       -> [ boardToWindow  row    (col+1)
                  , boardToWindow (row+1) (col+1) ]
  CBottomRight -> [ boardToWindow  row    (col+1)
                  , boardToWindow (row+1) (col+1)
                  , boardToWindow (row+1)  col    ]
  CBottom      -> [ boardToWindow (row+1)  col
                  , boardToWindow (row+1) (col+1) ]
  CBottomLeft  -> [ boardToWindow  row     col
                  , boardToWindow (row+1)  col
                  , boardToWindow (row+1) (col+1) ]
  CLeft        -> [ boardToWindow  row     col
                  , boardToWindow (row+1)  col    ]
  CNone        -> []
