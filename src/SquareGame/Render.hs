module SquareGame.Render (
    render
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Semigroup   ((<>))
import           Control.Lens
import           Graphics.Gloss
import           SquareGame
import           SquareGame.UI    (boardToWindow, shiftX, shiftY, squareCenter)
import           SquareGame.World


render :: World -> Picture
render world = picture
  where
    Board squares grid = world ^. board

    picture      = if world ^. squareToPlace == Nothing
                   then mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ cellHoveredOver
                               ++ deshroudableCells
                               ++ map renderPlaced (world ^. placed)
                               ++ pickup (world ^. squareToPickup)
                               ++ msg
                   else mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ cellHoveredOver
                               ++ placingSquare
                               ++ map renderPlaced (world ^. placed)
                               ++ msg

    full         = fullSquares squares

    pickup :: Maybe Square -> [Picture]
    pickup Nothing       = [Blank]
    pickup (Just square) = [Color blue $ renderFull square]

    renderPlaced :: Square -> Picture
    renderPlaced square = mconcat [Color green $ renderFull square]

    placingSquare :: [Picture]
    placingSquare = case world ^. squareToPlace of
                      Nothing     -> mempty
                      Just square -> [Color yellow $ renderFull square]


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
    renderFull square@(row, col, size) = line <> digit
      where
        line  = Line [ boardToWindow r     c
                     , boardToWindow (r+s) c
                     , boardToWindow (r+s) (c+s)
                     , boardToWindow r     (c+s)
                     , boardToWindow r     c ]

        digit = translateToSquareCenter square
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


translateToSquareCenter :: Square -> Picture -> Picture
translateToSquareCenter square = Translate x y
  where
    (x, y) = squareCenter square
