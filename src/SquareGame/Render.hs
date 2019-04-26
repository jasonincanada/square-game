module SquareGame.Render (
    render
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe       (isJust)
import           Data.Semigroup   ((<>))
import           Control.Lens
import           Graphics.Gloss
import           SquareGame
import           SquareGame.UI    (cellBorderPath, shiftX, shiftY, squareBorderPath, squareDigit)
import           SquareGame.World


render :: World -> Picture
render world = picture
  where
    Board squares grid = world ^. board

    picture      = mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ cellHoveredOver
                               ++ map renderPlaced (world ^. placed)
                               ++ pickup (world ^. squareToPickup)
                               ++ msg
                               ++ if isJust (world ^. squareToPlace)
                                    then placingSquare
                                    else deshroudableCells


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
    deshroudableCells = case world ^. cellsToClick of
                          Nothing    -> [Blank]
                          Just cells -> map (Color green . renderShroud) (S.toList cells)

    cellHoveredOver :: [Picture]
    cellHoveredOver = case world ^. cellHover of
      Nothing   -> [Blank]
      Just cell -> [ Color red $ Polygon (cellWholeBorderPath cell) ]

    cellWholeBorderPath :: Cell -> Path
    cellWholeBorderPath cell =  cellBorderPath cell CTopLeft
                             ++ cellBorderPath cell CBottomRight


    -- Render a fully-unshrouded square
    renderFull :: Square -> Picture
    renderFull square = line <> digit
      where
        line  = Line (squareBorderPath square)

        digit = translateToDigit square
                  $ Scale 0.2 0.2
                  $ Text (show $ size square)

    renderShroud :: Cell -> Picture
    renderShroud cell = Polygon (cellWholeBorderPath cell)

    renderUnshroud :: (Cell, CellBorder) -> Picture
    renderUnshroud (cell, border) = Line (cellBorderPath cell border)


    msg :: [Picture]
    msg = [ Translate (-350) 380
              $ Scale 0.2 0.2
              $ Text
              $ show (world ^. renderCount) ++ ", " ++ (world ^. message) ]

    shroud       = concatMap (S.toList . fst) (M.elems squares)
    unshroud     = concatMap (S.toList . snd) (M.elems squares)

    unshrouded   = map (\cell -> (cell, snd $ grid M.! cell)) unshroud


translateToDigit :: Square -> Picture -> Picture
translateToDigit square = Translate x y
  where
    (x, y) = squareDigit square
