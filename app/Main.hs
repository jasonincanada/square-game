{-# Language RecordWildCards #-}

{- Partridge Squares -}

module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Bifunctor (bimap)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact
import           SquareGame

file :: FilePath
file = "generation/squares/N8-888666688445522333178876768555777744.sqr"

data World = World { board :: Board
                   , message :: String
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
  let world = World board' "default message"

  play window white 20 world displayBoard events step

window :: Display
window = InWindow "Partridge Square" size position
  where
    size         = (windowWidth, windowHeight)
    position     = (100, 100)

events :: Event -> World -> World
events event world = case event of
  EventMotion (x, y) -> world { message = show x ++ "," ++ show y }
  _                  -> world

step :: Float -> World -> World
step float = id

boardToWindow :: Row -> Col -> (Float, Float)
boardToWindow row col = (x, y)
  where
    x = (fromIntegral col)    * boardscale + shiftX
    y = (fromIntegral (-row)) * boardscale + shiftY

displayBoard :: World -> Picture
displayBoard World{..} = picture
  where
    Board squares grid = board

    picture      = mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ msg

    full         = fullSquares squares

    renderFull :: Square -> Picture
    renderFull (row, col, size) =
      Line [ boardToWindow r     c
           , boardToWindow (r+s) c
           , boardToWindow (r+s) (c+s)
           , boardToWindow r     (c+s)
           , boardToWindow r     c
           ]
      where r = 2*row
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


squareScale, cellScale :: [(Int, Int)] -> Path
squareScale = fmap $ bimap ((*(2*boardscale)) . fromIntegral)
                           ((*(2*boardscale)) . fromIntegral)

cellScale   = fmap $ bimap ((*boardscale) . fromIntegral)
                           ((*boardscale) . fromIntegral)


 
