module SquareGame.Render (
    render
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Semigroup   ((<>))
import           Control.Lens
import           Graphics.Gloss
import           SquareGame
import           SquareGame.UI    (cellBorderPath, squareBorderPath, squareDigit)
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

digitScale   = 0.15
messageScale = 0.20

render :: World -> Picture
render world = picture
  where
    Board squares grid = world ^. board

    picture = mconcat (map renderFull     (fullSquares squares)
                    ++ map renderShroud   shroud
                    ++ map renderUnshroud unshrouded
                    ++ map renderPlaced   (world ^. placed))

                    <> maybeRender deshroudableCells   (world ^. cellsToClick)
                    <> maybeRender renderPlacingSquare (world ^. squareToPlace)
                    <> maybeRender pickup              (world ^. squareToPickup)
                    <> maybeRender cellHoveredOver     (world ^. cellHover)
                    <> boardMessage

    maybeRender :: (a -> Picture) -> Maybe a -> Picture
    maybeRender _      Nothing  = Blank
    maybeRender render (Just a) = render a


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
                   $ Scale digitScale digitScale
                   $ Text (show $ size square)


    renderShroud :: Cell -> Picture
    renderShroud cell = Polygon (cellWholeBorderPath cell)

    renderUnshroud :: (Cell, CellBorder) -> Picture
    renderUnshroud (cell, border) = Line (cellBorderPath cell border)


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
                         $ Scale digitScale digitScale
                         $ Text (show $ size square)


    renderPlacingSquare :: Square -> Picture
    renderPlacingSquare square = fill <> digit
      where
        fill   = Color (fade $ fade color) $ Polygon path
        path   = squareBorderPath square
        color  = colors M.! size square

        digit  = translateToDigit square
                   $ Color black
                   $ Scale digitScale digitScale
                   $ Text (show $ size square)


    deshroudableCells :: CellSet -> Picture
    deshroudableCells cells = mconcat $ map (Color green . renderShroud) (S.toList cells)

    pickup :: Square -> Picture
    pickup square = Color blue $ renderFull square

    cellHoveredOver :: Cell -> Picture
    cellHoveredOver cell = Color red $ Polygon (cellWholeBorderPath cell)

    cellWholeBorderPath :: Cell -> Path
    cellWholeBorderPath cell =  cellBorderPath cell CTopLeft
                             ++ cellBorderPath cell CBottomRight


    boardMessage :: Picture
    boardMessage = Translate (-350) 380
                     $ Scale messageScale messageScale
                     $ Text
                     $ show (world ^. renderCount) ++ ", " ++ (world ^. message)

    shroud       = S.toList shroudset
    shroudset    = foldr (S.union . fst) S.empty (M.elems squares)
    unshroud     = concatMap (S.toList . snd) (M.elems squares)
    unshrouded   = map (\cell -> (cell, snd $ grid M.! cell)) unshroud


translateToDigit :: Square -> Picture -> Picture
translateToDigit square = Translate x y
  where
    (x, y) = squareDigit square
