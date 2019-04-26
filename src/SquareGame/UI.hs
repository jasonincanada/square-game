module SquareGame.UI (
    boardScale
  , boardToWindow
  , clickables
  , shiftX
  , shiftY
  , windowToCell
  , windowWidth
  , windowHeight
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Helpers (minBy)
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


clickables :: Float -> Float -> Board -> CellSet
clickables x y (Board squares grid) = cells
  where
    cells = case windowToSquareEdge grid x y of
              Nothing             -> S.empty
              Just (square, edge) -> if fullyRevealed square
                                     then getFor square edge
                                     else S.empty

    fullyRevealed :: Square -> Bool
    fullyRevealed square = S.empty == fst (squares M.! square)

    getFor :: Square -> SquareSide -> CellSet
    getFor square edge = intersect
      where
        intersect = S.intersection all shrouded
        all       = click square edge
        shrouded  = foldr S.union S.empty (M.elems $ M.map fst squares)