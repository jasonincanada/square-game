module SquareGame.Render (
    render
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe       (fromJust, isJust)
import           Data.Semigroup   ((<>))
import           Control.Lens
import           Graphics.Gloss
import           SquareGame
import           SquareGame.UI    (cellBorderPath, shiftX, shiftY, squareBorderPath, squareDigit)
import           SquareGame.World

colors :: M.Map Size Color
colors = M.fromList [ (8, makeColor 0.9 0.9 0.9 solid)
                    , (7, makeColor 1.0 0.5 0.5 solid)
                    , (6, makeColor 1.0 1.0 0.6 solid)
                    , (5, makeColor 0.5 1.0 0.5 solid)
                    , (4, makeColor 0.6 0.8 1.0 solid)
                    , (3, makeColor 1.0 0.6 0.8 solid)
                    , (2, makeColor 0.9 0.5 0.0 solid)
                    , (1, makeColor 1.0 1.0 1.0 solid) ]
  where
    solid = 255

fade :: Color -> Color
fade = withAlpha 0.5


render :: World -> Picture
render world = picture
  where
    Board squares grid = world ^. board

    picture      = mconcat $ map renderFull full
                               ++ map renderShroud shroud
                               ++ map renderUnshroud unshrouded
                               ++ map renderPlaced (world ^. placed)
                               ++ if isJust (world ^. squareToPlace)
                                    then renderPlacingSquare (fromJust $ world ^. squareToPlace)
                                    else deshroudableCells
                               ++ pickup (world ^. squareToPickup)
                               ++ cellHoveredOver
                               ++ msg


    full         = fullSquares squares

    pickup :: Maybe Square -> [Picture]
    pickup Nothing       = [Blank]
    pickup (Just square) = [Color blue $ renderFull square]


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
    renderFull square = fill <> border <> digit
      where
        fill   = Color (bright $ bright color) $ Polygon path
        border = Line path
        path   = squareBorderPath square
        color  = colors M.! size square

        digit  = translateToDigit square
                   $ Color black
                   $ Scale 0.2 0.2
                   $ Text (show $ size square)


    -- Render a placed square, darkening the color of the shrouded cells (which become points for
    -- the player if they're placed correctly)
    renderPlaced :: Square -> Picture
    renderPlaced square = fillshroud <> fillunshroud <> border <> digit
      where
        fillshroud   = mconcat $ map (Color (dark   color) . Polygon . cellWholeBorderPath) shrouded
        fillunshroud = mconcat $ map (Color (bright color) . Polygon . cellWholeBorderPath) unshrouded

        shrouded     = S.toList $ S.intersection placedCells shroudset
        unshrouded   = S.toList $ S.difference   placedCells shroudset

        placedCells  = S.fromList $ map fst $ cells square

        border       = Line path
        path         = squareBorderPath square
        color        = colors M.! size square

        digit        = translateToDigit square
                         $ Color black
                         $ Scale 0.2 0.2
                         $ Text (show $ size square)


    renderPlacingSquare :: Square -> [Picture]
    renderPlacingSquare square = [ fill <> digit ]
      where
        fill   = Color (fade $ fade color) $ Polygon path
        path   = squareBorderPath square
        color  = colors M.! size square

        digit  = translateToDigit square
                   $ Color black
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
    shroudset    = foldr (S.union . fst) S.empty (M.elems squares)
    unshroud     = concatMap (S.toList . snd) (M.elems squares)

    unshrouded   = map (\cell -> (cell, snd $ grid M.! cell)) unshroud


translateToDigit :: Square -> Picture -> Picture
translateToDigit square = Translate x y
  where
    (x, y) = squareDigit square
