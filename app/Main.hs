{-# Language LambdaCase #-}
{-# Language RecordWildCards #-}

{- Partridge Squares -}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Bifunctor (bimap)
import           Graphics.Gloss
import           SquareGame

file :: FilePath
file = "generation/squares/N8-888666688445522333178876768555777744.sqr"

boardscale :: Float
boardscale = 10

main :: IO ()
main = do
  board <- deshroud (0,0,8) . deshroud (0,30,6) <$> fromFile file
  let clicked = click (0,0,8) SRight
  let board' = deshroudCells board (clicked ++ map fst (cells (0, 0, 36)))
  displayBoard board'

displayBoard :: Board -> IO ()
displayBoard Board{..} = display window color picture
  where
    window       = FullScreen -- InWindow "Partridge Square" size position
    size         = (floor $ boardscale * 2 * 36 + 100, floor $ boardscale * 2 * 36 + 100)
    position     = (100, 100)

    color        = white
    picture      = mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded

    full         = fullSquares squares

    renderFull (row, col, size)
                 = Line $ squareScale [ (col     , row     ),
                                        (col+size, row     ),
                                        (col+size, row+size),
                                        (col     , row+size),
                                        (col     , row     )]

    shroud       = concat . map S.toList $ map fst (M.elems squares)
    unshroud     = concat . map S.toList $ map snd (M.elems squares)

    unshrouded   = map (\cell -> (cell, snd $ grid M.! cell)) unshroud


squareScale, cellScale :: [(Int, Int)] -> Path
squareScale = fmap $ bimap ((*(2*boardscale)) . fromIntegral)
                           ((*(2*boardscale)) . fromIntegral)

cellScale   = fmap $ bimap ((*boardscale) . fromIntegral)
                           ((*boardscale) . fromIntegral)

renderShroud :: Cell -> Picture
renderShroud (row, col) = Polygon path
  where
    path = cellScale [ (col,   row  ),
                       (col+1, row  ),
                       (col+1, row+1),
                       (col,   row+1) ]

renderUnshroud :: (Cell, CellBorder) -> Picture
renderUnshroud ((row, col), border) = case border of
  CTopLeft     -> Line $ cellScale [ (col+1, row  ),
                                     (col  , row  ),
                                     (col  , row+1)]
  CTop         -> Line $ cellScale [ (col,   row  ),
                                     (col+1, row  )]
  CTopRight    -> Line $ cellScale [ (col  , row  ),
                                     (col+1, row  ),
                                     (col+1, row+1)]
  CRight       -> Line $ cellScale [ (col+1, row  ),
                                     (col+1, row+1)]
  CBottomRight -> Line $ cellScale [ (col+1, row  ),
                                     (col+1, row+1),
                                     (col  , row+1)]
  CBottom      -> Line $ cellScale [ (col  , row+1),
                                     (col+1, row+1)]
  CBottomLeft  -> Line $ cellScale [ (col  , row  ),
                                     (col  , row+1),
                                     (col+1, row+1)]
  CLeft        -> Line $ cellScale [ (col  , row  ),
                                     (col  , row+1)]
  CNone        -> Blank
 
